//! # Pipes
//!
//! This is a small crate that lets you build pipe chains between spawned
//! processes using a syntax similar to the Unix shell.
//!
//! ```
//! # #[macro_use] extern crate pipes;
//! # use pipes::IntoPipeSetup;
//! 
//! let output = spawn!(
//! 		cat <"./tests/input.txt" |
//! 		grep "spam" |
//! 		wc "-l"
//! 	).expect("spawn failed").
//! 	output().
//! 	expect("reading output failed").
//! 	stdout;
//! 
//! let output = String::from_utf8(output).
//! 	expect("UTF-8 error").
//! 	trim().
//! 	parse::<i64>().
//! 	unwrap();
//! 
//! println!("output: {}", output);
//! ```
//! 
//! Note that arguments to commands need always to be quoted. If you don't quote
//! them they are interpreted as Rust identifiers which allows you to pass
//! dynamic strings. Also `FOO="bar" cat <"baz"` is the same as
//! `FOO = "bar" cat < "baz"`, since whitespace is ignored in Rust.
//! 
//! `use pipes::IntoPipeSetup` is needed when passing a file name or
//! `std::fs::File` as redirection source/target.
//! 
//! **Note:** Currently only Linux ist tested. Other Unix operating systems might
//! work, too. Windows support is not implemented.
//! 
//! ## More Examples
//! 
//! ```
//! # #[macro_use] extern crate pipes;
//! # use pipes::IntoPipeSetup;
//! # use std::fs::File;
//! # use std::io::{Read, Seek, SeekFrom};
//! use pipes::PipeSetup::*;
//! 
//! // Ignore stderr
//! spawn!(rm "no_such_file" 2>Null);
//! 
//! // Redirect stderr to stdout
//! spawn!(rm "no_such_file" 2>&1);
//! 
//! // Write stderr to stderr of this process
//! spawn!(rm "no_such_file" 2>Inherit);
//! 
//! // Read from a file opened in Rust
//! let file = File::open("./tests/input.txt").
//! 	expect("couldn't open file");
//! spawn!(grep "spam" <file);
//! 
//! // Write stderr to a temp file
//! let mut chain = spawn!(rm "no_such_file" 2>Temp).
//! 	expect("spawn failed");
//! 
//! chain.wait_last().
//! 	expect("wait failed");
//! 
//! let mut temp = chain.stderr().unwrap();
//! 
//! // Since the file descriptor was shared with the spawned process the
//! // position needs to be reset manually:
//! temp.seek(SeekFrom::Start(0)).
//! 	expect("seek failed");
//! ```

extern crate libc;

use std::vec::Vec;
use std::result;
use std::os::unix::io::{FromRawFd, IntoRawFd, AsRawFd};
use std::collections::HashMap;
use std::ffi::{CStr};
use std::fmt::Display;
use std::fs::File;
use std::ptr;
use std::env;
use std::mem::drop;
use std::io::Read;

use libc::{
	fork,
	pipe,
	strerror_r, // XSI
	perror,
	open,
	close,
	dup2,
	pid_t,
	execvp,
	mkstemp,
	unlink,
	exit,
	kill,
	SIGTERM,
	waitpid,
	WIFEXITED,
	WEXITSTATUS,
	WIFSIGNALED,
	WTERMSIG,
	WCOREDUMP,
	WIFSTOPPED,
	WSTOPSIG,
	WIFCONTINUED,
	c_int,
	c_char,
	__errno_location, // Linux?
	O_RDONLY,
	O_WRONLY,
	O_APPEND,
	O_CREAT,
	O_TRUNC,
	O_RDWR,
	S_IRUSR,
	S_IWUSR,
	S_IRGRP,
	S_IWGRP,
	S_IROTH,
	EOPNOTSUPP,
	EISDIR,
	ENOENT,
	ECHILD,
	EINVAL,
	EINTR,
	EXIT_FAILURE,
	STDIN_FILENO,
	STDOUT_FILENO,
	STDERR_FILENO,
	BUFSIZ,
	poll,
	pollfd,
	POLLIN,
	POLLERR,
	POLLHUP,
	POLLNVAL
};

#[cfg(target_os = "macos")]
#[inline]
unsafe fn environ() -> *mut *const *const c_char {
	extern { fn _NSGetEnviron() -> *mut *const *const c_char; }
	_NSGetEnviron()
}

#[cfg(not(target_os = "macos"))]
#[inline]
unsafe fn environ() -> *mut *const *const c_char {
	extern { static mut environ: *const *const c_char; }
	&mut environ
}

macro_rules! cstr {
	($str:expr) => {
		($str).as_ptr() as *const c_char
	}
}

/// Redirection target. Used with [PipeSetup].
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Target {
	Stdout,
	Stderr
}

/// File open mode. Used with [PipeSetup].
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Mode {
	Read,
	Write,
	Append
}

/// Setup a pipe.
#[derive(Debug)]
pub enum PipeSetup {
	/// Inherit the stream from the parent process. This is the default for
	/// stderr and for stdin of the first process in the pipe chain.
	Inherit,

	/// Open a pipe to the parent process or between processes. This is the
	/// default for stdout and for stdin of all but the first process in the
	/// pipe chain.
	Pipe,

	/// Open a pipe to `/dev/null`.
	Null,

	/// Redirect this stream to the specified target stream. Only stdout and
	/// stderr can be redirected this way and only to each other. If a stream
	/// is redirected to itself it is equivalent to [PipeSetup::Pipe].
	Redirect(Target),

	/// Connect the stream to a temp file. If supported this is done via the
	/// operating systems native temp file support using the `O_TMPFILE` flag.
	/// Such a file is written to the hard disk, but has no name and will be
	/// deleted after the file handle to it is closed.
	///
	/// If the operating system or filesystem of `/tmp` does not support the
	/// `O_TMPFILE` flag the same behaviour is emulated by opening a new file
	/// using `mkstemp()` and then immediately `unlink()`ed.
	Temp,

	/// Connect the stream to the specified file descriptor. Note that error
	/// or not any passed file descriptor will be consumed (closed) by
	/// [Chain::new()].
	FileDescr(c_int),

	/// Connect the stream to the specified file using the specified mode.
	FileName(String, Mode),

	/// Connect the stream to the specified file. Note that error or not any
	/// passed file will be consumed (closed) by [Chain::new()].
	File(File)
}

impl PipeSetup {
	pub fn is_redirect(&self) -> bool {
		match self {
			PipeSetup::Redirect(_) => true,
			_ => false
		}
	}

	pub fn is_redirect_to(&self, target: Target) -> bool {
		match self {
			PipeSetup::Redirect(actual) => target == *actual,
			_ => false
		}
	}

	pub fn is_inherit(&self) -> bool {
		match self {
			PipeSetup::Inherit => true,
			_ => false
		}
	}

	pub fn is_pipe(&self) -> bool {
		match self {
			PipeSetup::Pipe => true,
			_ => false
		}
	}

	pub fn is_null(&self) -> bool {
		match self {
			PipeSetup::Null => true,
			_ => false
		}
	}

	pub fn is_temp(&self) -> bool {
		match self {
			PipeSetup::Temp => true,
			_ => false
		}
	}

	pub fn is_any_file(&self) -> bool {
		match self {
			PipeSetup::File(_) | PipeSetup::FileDescr(_) | PipeSetup::FileName(_, _) => true,
			_ => false
		}
	}

	pub fn is_file(&self) -> bool {
		match self {
			PipeSetup::File(_) => true,
			_ => false
		}
	}

	pub fn is_file_descr(&self) -> bool {
		match self {
			PipeSetup::FileDescr(_) => true,
			_ => false
		}
	}

	pub fn is_file_name(&self) -> bool {
		match self {
			PipeSetup::FileName(_, _) => true,
			_ => false
		}
	}

	unsafe fn into_raw_fd(self) -> c_int {
		match self {
			PipeSetup::Inherit => PIPES_INHERIT,
			PipeSetup::Pipe => PIPES_PIPE,
			PipeSetup::Null => PIPES_NULL,
			PipeSetup::Redirect(Target::Stdout) => PIPES_TO_STDOUT,
			PipeSetup::Redirect(Target::Stderr) => PIPES_TO_STDERR,
			PipeSetup::Temp => PIPES_TEMP,
			PipeSetup::FileDescr(fd) => fd,
			PipeSetup::FileName(name, mode) => {
				let mut name = name.into_bytes();
				name.push(0);

				let fd = match mode {
					Mode::Read   => open(cstr!(name), O_RDONLY),
					Mode::Write  => open(cstr!(name), O_WRONLY | O_CREAT | O_TRUNC,  S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH),
					Mode::Append => open(cstr!(name), O_WRONLY | O_CREAT | O_APPEND, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH),
				};

				fd
			}
			PipeSetup::File(file) => file.into_raw_fd()
		}
	}
}

/// Configuration of a child process.
#[derive(Debug)]
pub struct Pipes {
	pub stdin:  PipeSetup,
	pub stdout: PipeSetup,
	pub stderr: PipeSetup,
	pub last: Target,
	pub argv: Vec<String>,
	pub envp: HashMap<String, String>
}

#[derive(Debug)]
struct ChildIntern {
	pid:   pid_t,
	infd:  c_int,
	outfd: c_int,
	errfd: c_int,
}

/// Represents a pipe chain.
#[derive(Debug)]
pub struct Chain {
	children: Vec<ChildIntern>
}

pub enum Error {
	/// A libc function returned an error of specified `errno`.
	OS(c_int),

	/// Any command needs to have at least one "argument" (the command name).
	NotEnoughArguments,

	/// stdin cannot be redirected to stderr or stdout, since those are output
	/// streams and stdin is an input stream.
	CannotRedirectStdinTo(Target),

	/// A pipe chain has to have at least one command.
	NotEnoughPipes,

	/// If stdin of a child process is marked as `PipeSetup::Pipe` then stdout
	/// of the previous process in the chain has to be `PipeSetup::Pipe` or
	/// `PipeSetup::Redirect(Target::Stdout)` or stderr of that previous
	/// process has to be `PipeSetup::Redirect(Target::Stdout)`.
	InvalidPipeLinkup
}

pub enum WaitError {
	Interrupted,
	ChildInvalid,
	ChildSignaled(c_int),
	ChildCoreDumped,
	ChildStopped(c_int),
	ChildContinued,
}

pub enum OutputError {
	/// A Rust io function returned the specified error.
	IO(std::io::Error),
	Pipe(Error),
	Wait(WaitError)
}

/// Output and exit status of the last process in the chain.
pub struct Output {
	pub status: c_int,
	pub stdout: Vec<u8>,
	pub stderr: Vec<u8>
}

pub type Result<T> = result::Result<T, Error>;

pub type WaitResult = result::Result<c_int, WaitError>;

pub trait IntoPipeSetup {
	fn into_pipe_setup(self, mode: Mode) -> PipeSetup;
}

impl PipeSetup {
	pub fn into_pipe_setup(self, mode: Mode) -> PipeSetup {
		match self {
			PipeSetup::FileName(name, _) => PipeSetup::FileName(name, mode),
			setup => setup
		}
	}
}

impl IntoPipeSetup for PipeSetup {
	fn into_pipe_setup(self, mode: Mode) -> PipeSetup {
		match self {
			PipeSetup::FileName(name, _) => PipeSetup::FileName(name, mode),
			setup => setup
		}
	}
}

impl<'a> IntoPipeSetup for &'a str {
	fn into_pipe_setup(self, mode: Mode) -> PipeSetup {
		PipeSetup::FileName(self.to_string(), mode)
	}
}

impl<'a> IntoPipeSetup for &'a String {
	fn into_pipe_setup(self, mode: Mode) -> PipeSetup {
		PipeSetup::FileName(self.clone(), mode)
	}
}

impl IntoPipeSetup for File {
	fn into_pipe_setup(self, _mode: Mode) -> PipeSetup {
		PipeSetup::File(self)
	}
}

macro_rules! c_err {
	() => {
		Err(Error::OS(unsafe { *__errno_location() }))
	}
}

fn fmt_os_error(errno: c_int, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
	write!(f, "OS error code {}: ", errno)?;

	let mut buf = [0u8; BUFSIZ as usize];
	let res: c_int = unsafe {
		strerror_r(errno, buf.as_mut_ptr() as *mut c_char, buf.len())
	};
	if res != 0 {
		return write!(f,
			"(Another OS error (code {}) occured getting the error string)",
			if res > 0 { res } else { unsafe { *__errno_location() } });
	}

	let index = buf.iter().position(|b| *b == 0);
	let index = match index {
		Some(index) => index,
		None => {
			return write!(f, "(Error getting error string: no nul byte in error string)");
		}
	};

	match CStr::from_bytes_with_nul(&buf[..=index]) {
		Ok(errstr) => {
			match errstr.to_str() {
				Ok(s) => {
					return f.write_str(s);
				},
				Err(err) => {
					return write!(f, "(Error getting error string: {})", err);
				}
			}
		},
		Err(err) => {
			return write!(f, "(Error getting error string: {})", err);
		}
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
		match self {
			Error::OS(errno) => fmt_os_error(*errno, f),
			Error::NotEnoughArguments => f.write_str("not enough arguments (need at least one)"),
			Error::CannotRedirectStdinTo(Target::Stdout) => f.write_str("cannot redirect stdin to stdout"),
			Error::CannotRedirectStdinTo(Target::Stderr) => f.write_str("cannot redirect stdin to stderr"),
			Error::NotEnoughPipes => f.write_str("not enough pipes (need at least one)"),
			Error::InvalidPipeLinkup => f.write_str("invalid pipe linkup"),
		}
	}
}

impl Display for WaitError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
		match self {
			WaitError::Interrupted => write!(f, "wait was interrupted"),
			WaitError::ChildInvalid => write!(f, "process does not exist or is not a child of calling process"),
			WaitError::ChildSignaled(sig) => write!(f, "child exited with signal: {}", sig),
			WaitError::ChildCoreDumped => f.write_str("child core dumped"),
			WaitError::ChildStopped(sig) => write!(f, "child stopped with signal: {}", sig),
			WaitError::ChildContinued => f.write_str("child continued"),
		}
	}
}

impl Display for OutputError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
		match self {
			OutputError::IO(err)   => err.fmt(f),
			OutputError::Pipe(err) => err.fmt(f),
			OutputError::Wait(err) => err.fmt(f)
		}
	}
}

impl std::fmt::Debug for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
		(self as &Display).fmt(f)
	}
}

impl std::fmt::Debug for WaitError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
		(self as &Display).fmt(f)
	}
}

impl std::fmt::Debug for OutputError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
		(self as &Display).fmt(f)
	}
}

#[macro_export]
#[doc(hidden)]
macro_rules! sapwn_unexpected_end {
	($tt:tt) => {}
}

#[macro_export]
#[doc(hidden)]
macro_rules! sapwn_unexpected_token {
	() => {}
}

#[macro_export]
#[doc(hidden)]
macro_rules! sapwn_illegal_mode {
	() => {}
}

#[macro_export]
#[doc(hidden)]
macro_rules! spawn_internal_envp {
	($((($($key:tt)*) ($($val:tt)*)))*) => {
		{
			let mut envp = std::collections::HashMap::new();
			$(envp.insert($($key)*.to_string(), $($val)*.to_string());)*
			envp
		}
	}
}

#[macro_export]
#[doc(hidden)]
macro_rules! spawn_internal {
	(stdin ($($stream:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($last:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($cont:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($($cont)* (($($stream)*) ($($stdout)*) ($($stderr)*) ($($last)*) ($($argv)*) ($($envp)*)) ($($chain)*))
	};

	(stdout ($($stream:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($last:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($cont:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($($cont)* (($($stdin)*) ($($stream)*) ($($stderr)*) (pipes::Target::Stdout) ($($argv)*) ($($envp)*)) ($($chain)*))
	};

	(stderr ($($stream:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($last:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($cont:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($($cont)* (($($stdin)*) ($($stdout)*) ($($stream)*) (pipes::Target::Stderr) ($($argv)*) ($($envp)*)) ($($chain)*))
	};

	(@arg ($($arg:tt)*) ($($rest:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($last:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($chain:tt)*)) => {
		spawn_internal!(@body () ($($rest)*) (($($stdin)*) ($($stdout)*) ($($stderr)*) ($($last)*) ($($argv)* ($($arg)*)) ($($envp)*)) ($($chain)*))
	};

	(@var ($($key:tt)*) ($($val:tt)*) ($($rest:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($last:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($chain:tt)*)) => {
		spawn_internal!(@head () ($($rest)*) (($($stdin)*) ($($stdout)*) ($($stderr)*) ($($last)*) ($($argv)*) ($($envp)* (($($key)*) ($($val)*)))) ($($chain)*))
	};

	// ========== ENVIRONMENT VARIABLES ========================================
	(@env ($key:ident) ($val:expr) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@var (stringify!($key)) ($val) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@env ($key:ident) ($($tt:tt)*) ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@env ($key) ($($tt)* $tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	// ========== HEAD =========================================================
	(@head ($id:ident) (= $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@env ($id) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@head ($id:ident) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@arg (stringify!($id)) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@head ($id:expr) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@arg ($id) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@head () ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@head ($tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@head ($tt:tt) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		sapwn_unexpected_token!($tt)
	};

	// ========== HELPER =======================================================
	(@end ($($chain:tt)*)) => {
		vec![$($chain),*]
	};

	(@chain ($($elem:tt)*) ($($chain:tt)*) ($($cont:tt)*)) => {
		spawn_internal!($($cont)* ($($chain)* ($($elem)*)))
	};

	// ========== BODY =========================================================
	(@body () () (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($last:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($chain:tt)*)) => {
		spawn_internal!(@chain (pipes::Pipes {
			stdin: $($stdin)*,
			stdout: $($stdout)*,
			stderr: $($stderr)*,
			last: $($last)*,
			argv: vec![$($argv.to_string()),*],
			envp: spawn_internal_envp!($($envp)*)
		}) ($($chain)*) (@end))
	};

	(@body (|) () ($($opts:tt)*) ($($chain:tt)*)) => {
		sapwn_unexpected_end!()
	};

	(@body (|) (| $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		sapwn_unexpected_token!(|)
	};

	(@body (|) ($($rest:tt)+) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($last:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($chain:tt)*)) => {
		spawn_internal!(@chain
			(pipes::Pipes {
				stdin: $($stdin)*,
				stdout: $($stdout)*,
				stderr: $($stderr)*,
				last: $($last)*,
				argv: vec![$($argv.to_string()),*],
				envp: spawn_internal_envp!($($envp)*)
			})
			($($chain)*)
			(@head () ($($rest)+) ((pipes::PipeSetup::Pipe) (pipes::PipeSetup::Pipe) (pipes::PipeSetup::Inherit) (pipes::Target::Stderr) () ()))
		)
	};

	(@body (<) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe stdin (pipes::Mode::Read) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body (0) (< $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe stdin (pipes::Mode::Read) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body () (>> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe stdout (pipes::Mode::Append) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body (1) (>> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe stdout (pipes::Mode::Append) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body (2) (>> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe stderr (pipes::Mode::Append) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body () (> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe stdout (pipes::Mode::Write) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body (1) (> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe stdout (pipes::Mode::Write) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body (2) (> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe stderr (pipes::Mode::Write) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body ($arg:expr) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@arg ($arg) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body ($($tt:tt)*) ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@body ($($tt)* $tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	// ========== PIPE =========================================================
	(@pipe $pipe:ident (pipes::Mode::Write) (&) (1 $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($pipe (pipes::PipeSetup::Redirect(pipes::Target::Stdout)) ($($opts)*) (@body () ($($rest)*)) ($($chain)*))
	};

	(@pipe $pipe:ident (pipes::Mode::Write) (&) (2 $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($pipe (pipes::PipeSetup::Redirect(pipes::Target::Stderr)) ($($opts)*) (@body () ($($rest)*)) ($($chain)*))
	};

	(@pipe $pipe:ident (pipes::Mode::Write) (&) ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		sapwn_unexpected_token!($tok)
	};

	(@pipe $pipe:ident ($($mode:tt)*) (&) ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		sapwn_illegal_mode!($mode)
	};

	(@pipe $pipe:ident ($($mode:tt)*) ($($tt:tt)*) (:: $tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe $pipe ($($mode)*) ($($tt)* :: $tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@pipe $pipe:ident ($($mode:tt)*) ($($tt:tt)*) (. $tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe $pipe ($($mode)*) ($($tt)* . $tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@pipe $pipe:ident ($($mode:tt)*) ($io:expr) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($pipe ($io.into_pipe_setup($($mode)*)) ($($opts)*) (@body () ($($rest)*)) ($($chain)*))
	};

	(@pipe $pipe:ident ($($mode:tt)*) ($($tt:tt)*) ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe $pipe ($($mode)*) ($($tt)* $tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};
}

/// Create a pipe chain using a Unix shell like syntax.
#[macro_export]
macro_rules! spawn {
	($($tt:tt)*) => {
		pipes::Chain::new(
			// arguments: @marker (current token) (tokens to parse) ((stdin) (stdout) (stderr) (last) (argv) (envp)) (pipe chain array)
			spawn_internal!(@head () ($($tt)*) ((pipes::PipeSetup::Inherit) (pipes::PipeSetup::Pipe) (pipes::PipeSetup::Inherit) (pipes::Target::Stderr) () ()) ()))
	}
}

fn open_temp_fd_fallback() -> c_int {
	let mut name: [u8; 17] = *b"/tmp/pipesXXXXXX\0";
	let fd = unsafe { mkstemp(name.as_mut_ptr() as *mut c_char) };
	if fd < 0 {
		return -1;
	}

	if unsafe { unlink(cstr!(name)) } != 0 {
		unsafe { close(fd); }
		return -1;
	}
	return -1;
}

fn open_temp_fd() -> c_int {
	if cfg!(target_os = "linux") {
		let fd = unsafe { open(cstr!(b"/tmp\0"), libc::O_TMPFILE | O_RDWR, S_IRUSR | S_IWUSR) };
		if fd < 0 {
			let errno = unsafe { *__errno_location() };
			match errno {
				EOPNOTSUPP | EISDIR | ENOENT => return open_temp_fd_fallback(),
				_ => {}
			}
		}

		return fd;
	} else {
		return open_temp_fd_fallback();
	}
}

fn redirect_fd(oldfd: c_int, newfd: c_int, errmsg: *const c_char) {
	if oldfd > -1 && oldfd != newfd {
		if unsafe { dup2(oldfd, newfd) } == -1 {
			unsafe {
				perror(errmsg);
				exit(EXIT_FAILURE);
			}
		}

		unsafe { close(oldfd); }
	}
}

fn redirect_out_fd(action: c_int, oldfd: c_int, newfd: c_int, errmsg: *const c_char) {
	match action {
		::PIPES_TO_STDOUT => redirect_fd(STDOUT_FILENO, newfd, errmsg),
		::PIPES_TO_STDERR => redirect_fd(STDERR_FILENO, newfd, errmsg),
		_                 => redirect_fd(oldfd, newfd, errmsg),
	}
}

const PIPES_INHERIT:   c_int = -2;
const PIPES_PIPE:      c_int = -3;
const PIPES_NULL:      c_int = -4;
const PIPES_TO_STDOUT: c_int = -5;
const PIPES_TO_STDERR: c_int = -6;
const PIPES_TEMP:      c_int = -7;

fn pipes_close(child: &mut ChildIntern) -> c_int {
	unsafe {
		let mut status = 0;

		if child.infd > -1 {
			if close(child.infd) != 0 {
				status = -1;
			}
			child.infd = -1;
		}

		if child.outfd > -1 {
			if close(child.outfd) != 0 {
				status = -1;
			}
			child.outfd = -1;
		}

		if child.errfd > -1 {
			if close(child.errfd) != 0 {
				status = -1;
			}
			child.errfd = -1;
		}

		return status;
	}
}

fn handle_error(infd: c_int, outfd: c_int, errfd: c_int, child: &mut ChildIntern) -> Result<()> {
	unsafe {
		let errno = *__errno_location();

		if infd  > -1 { close(infd); }
		if outfd > -1 { close(outfd); }
		if errfd > -1 { close(errfd); }

		pipes_close(child);

		return Err(Error::OS(errno));
	}
}

fn pipes_open(argv: *const *const c_char, envp: *const *const c_char, child: &mut ChildIntern, last: Target) -> Result<()> {
	unsafe {
		let mut infd  = -1;
		let mut outfd = -1;
		let mut errfd = -1;

		let inaction  = child.infd;
		let outaction = child.outfd;
		let erraction = child.errfd;

		match inaction {
			::PIPES_PIPE => {
				let mut pair: [c_int; 2] = [-1, -1];
				if pipe(pair.as_mut_ptr()) == -1 {
					return handle_error(infd, outfd, errfd, child);
				}
				infd = pair[0];
				child.infd = pair[1];
			},
			::PIPES_NULL => {
				infd = open(cstr!(b"/dev/null\0"), O_RDONLY);

				if infd < 0 {
					return handle_error(infd, outfd, errfd, child);
				}
			},
			::PIPES_TEMP => {
				infd = open_temp_fd();
				child.infd = infd;

				if infd < 0 {
					return handle_error(infd, outfd, errfd, child);
				}
			},
			::PIPES_INHERIT => {},
			_ if inaction > -1 => {
				infd = inaction;
				child.infd = -1;
			},
			_ => {
				*__errno_location() = EINVAL;
				return handle_error(infd, outfd, errfd, child);
			}
		}

		match outaction {
			::PIPES_PIPE => {
				let mut pair: [c_int; 2] = [-1, -1];
				if pipe(pair.as_mut_ptr()) == -1 {
					return handle_error(infd, outfd, errfd, child);
				}
				outfd = pair[1];
				child.outfd = pair[0];
			},
			::PIPES_NULL => {
				outfd = open(cstr!(b"/dev/null\0"), O_RDONLY);

				if outfd < 0 {
					return handle_error(infd, outfd, errfd, child);
				}
			},
			::PIPES_TO_STDERR => {},
			::PIPES_TEMP => {
				outfd = open_temp_fd();
				child.outfd = outfd;

				if outfd < 0 {
					return handle_error(infd, outfd, errfd, child);
				}
			},
			::PIPES_INHERIT => {},
			_ if outaction > -1 => {
				outfd = outaction;
				child.outfd = -1;
			},
			_ => {
				*__errno_location() = EINVAL;
				return handle_error(infd, outfd, errfd, child);
			}
		}

		match erraction {
			::PIPES_PIPE => {
				let mut pair: [c_int; 2] = [-1, -1];
				if pipe(pair.as_mut_ptr()) == -1 {
					return handle_error(infd, outfd, errfd, child);
				}
				errfd = pair[1];
				child.errfd = pair[0];
			},
			::PIPES_NULL => {
				errfd = open(cstr!(b"/dev/null\0"), O_RDONLY);

				if errfd < 0 {
					return handle_error(infd, outfd, errfd, child);
				}
			},
			::PIPES_TO_STDOUT => {},
			::PIPES_TEMP => {
				errfd = open_temp_fd();
				child.errfd = errfd;

				if errfd < 0 {
					return handle_error(infd, outfd, errfd, child);
				}
			},
			::PIPES_INHERIT => {},
			_ if erraction > -1 => {
				errfd = erraction;
				child.errfd = -1;
			},
			_ => {
				*__errno_location() = EINVAL;
				return handle_error(infd, outfd, errfd, child);
			}
		}

		let pid = fork();

		if pid == -1 {
			return handle_error(infd, outfd, errfd, child);
		}

		if pid == 0 {
			// child
			redirect_fd(infd, STDIN_FILENO, cstr!(b"redirecting stdin\0"));

			if last == Target::Stderr {
				redirect_out_fd(outaction, outfd, STDOUT_FILENO, cstr!(b"redirecting stdout\0"));
				redirect_out_fd(erraction, errfd, STDERR_FILENO, cstr!(b"redirecting stderr\0"));
			} else {
				redirect_out_fd(erraction, errfd, STDERR_FILENO, cstr!(b"redirecting stderr\0"));
				redirect_out_fd(outaction, outfd, STDOUT_FILENO, cstr!(b"redirecting stdout\0"));
			}

			if envp != ptr::null() {
				*environ() = envp;
			}

			if execvp(*argv, argv) == -1 {
				perror(*argv);
			}
			exit(EXIT_FAILURE);
		} else {
			// parent
			child.pid = pid;

			if inaction  != PIPES_TEMP && infd  > -1 { close(infd); }
			if outaction != PIPES_TEMP && outfd > -1 { close(outfd); }
			if erraction != PIPES_TEMP && errfd > -1 { close(errfd); }
		}

		Ok(())
	}
}

fn intern_wait(pid: pid_t) -> WaitResult {
	if pid <= 0 {
		return Err(WaitError::ChildInvalid);
	}

	let mut status: c_int = -1;
	if unsafe { waitpid(pid, &mut status, 0) } == -1 {
		return Err(match unsafe { *__errno_location() } {
			::ECHILD => WaitError::ChildInvalid,
			::EINTR => WaitError::Interrupted,
			errno => panic!("unhandled errno: {}", errno)
		});
	}

	if unsafe { WIFEXITED(status) } {
		return Ok(unsafe { WEXITSTATUS(status) });
	}

	if unsafe { WIFSIGNALED(status) } {
		return Err(WaitError::ChildSignaled(unsafe { WTERMSIG(status) }));
	}

	if unsafe { WCOREDUMP(status) } {
		return Err(WaitError::ChildCoreDumped);
	}

	if unsafe { WIFSTOPPED(status) } {
		return Err(WaitError::ChildStopped(unsafe { WSTOPSIG(status) }));
	}

	if unsafe { WIFCONTINUED(status) } {
		return Err(WaitError::ChildContinued);
	}

	return Err(WaitError::ChildInvalid);

}

impl Pipes {
	pub fn empty() -> Self {
		Pipes {
			stdin:  PipeSetup::Inherit,
			stdout: PipeSetup::Inherit,
			stderr: PipeSetup::Inherit,
			last: Target::Stderr,
			argv: Vec::with_capacity(0),
			envp: HashMap::with_capacity(0)
		}
	}

	pub fn new(prog: &str) -> Self {
		Pipes {
			stdin:  PipeSetup::Inherit,
			stdout: PipeSetup::Inherit,
			stderr: PipeSetup::Inherit,
			last: Target::Stderr,
			argv: vec![prog.to_string()],
			envp: HashMap::new()
		}
	}

	pub fn first(args: &[&str]) -> Self {
		Pipes {
			stdin:  PipeSetup::Inherit,
			stdout: PipeSetup::Pipe,
			stderr: PipeSetup::Inherit,
			last: Target::Stderr,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: HashMap::new()
		}
	}

	pub fn last(args: &[&str]) -> Self {
		Pipes {
			stdin:  PipeSetup::Pipe,
			stdout: PipeSetup::Inherit,
			stderr: PipeSetup::Inherit,
			last: Target::Stderr,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: HashMap::new()
		}
	}

	pub fn pass_through(args: &[&str]) -> Self {
		Pipes {
			stdin:  PipeSetup::Pipe,
			stdout: PipeSetup::Pipe,
			stderr: PipeSetup::Inherit,
			last: Target::Stderr,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: HashMap::new()
		}
	}

	pub fn pass_stdin(fd: c_int, args: &[&str]) -> Self {
		Pipes {
			stdin:  PipeSetup::FileDescr(fd),
			stdout: PipeSetup::Pipe,
			stderr: PipeSetup::Inherit,
			last: Target::Stderr,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: HashMap::new()
		}
	}

	pub fn pass_stdout(fd: c_int, args: &[&str]) -> Self {
		Pipes {
			stdin:  PipeSetup::Pipe,
			stdout: PipeSetup::FileDescr(fd),
			stderr: PipeSetup::Inherit,
			last: Target::Stderr,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: HashMap::new()
		}
	}

	pub fn pass_stderr(fd: c_int, args: &[&str]) -> Self {
		Pipes {
			stdin:  PipeSetup::Pipe,
			stdout: PipeSetup::Inherit,
			stderr: PipeSetup::FileDescr(fd),
			last: Target::Stderr,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: HashMap::new()
		}
	}

	pub fn has_output(&self) -> bool {
		// cases in which nothing arrives at stdout:
		// 2>&2 1>&2
		// 1>&2 2>&1
		// 1>&2 2>&2
		// 1>$FILE 2>&1
		match self.stdout {
			PipeSetup::Pipe => {},
			PipeSetup::FileDescr(_) | PipeSetup::FileName(_, _) => {
				if self.last == Target::Stderr || self.stderr.is_inherit() {
					return false;
				}
			},
			PipeSetup::Redirect(Target::Stderr) => {
				if
						(self.last == Target::Stderr && self.stderr.is_redirect_to(Target::Stdout)) ||
						self.stderr.is_redirect_to(Target::Stderr) {
					return false;
				}
			},
			_ => {}
		}
		return true;
	}

	pub fn env(&mut self, key: &str, value: &str) {
		self.envp.insert(key.to_string(), value.to_string());
	}

	pub fn arg(&mut self, arg: &str) {
		self.argv.push(arg.to_string());
	}

	pub fn stdin(&mut self, setup: PipeSetup) {
		match setup {
			PipeSetup::Redirect(_) => {},
			_ => {
				self.stdin = setup;
			}
		}
	}

	pub fn stdout(&mut self, setup: PipeSetup) {
		self.stdout = setup;
		self.last = Target::Stdout;
	}

	pub fn stderr(&mut self, setup: PipeSetup) {
		self.stderr = setup;
		self.last = Target::Stderr;
	}
}

fn make_ptr_array(buf: &Vec<u8>, len: usize) -> Vec<*const c_char> {
	let mut ptrs = Vec::<*const c_char>::with_capacity(len);
	let mut index = 0;
	if buf[0] != 0 {
		let len = buf.len();
		ptrs.push(cstr!(buf[..]));
		while index < len {
			if buf[index] == 0 && index + 1 < len {
				ptrs.push(cstr!(buf[index + 1..]));
			}
			index += 1;
		}
	}
	ptrs.push(ptr::null());
	ptrs
}

fn make_argv(argv: &Vec<String>) -> (Vec<u8>, Vec<*const c_char>) {
	let mut buf = Vec::<u8>::new();
	for arg in argv {
		buf.extend_from_slice(arg.as_bytes());
		buf.push(0);
	}

	let argv = make_ptr_array(&buf, argv.len());
	(buf, argv)
}

fn make_envp(vars: &HashMap<String, String>) -> (Vec<u8>, Vec<*const c_char>) {
	let mut envp = HashMap::<Vec<u8>, Vec<u8>>::new();
	for (key, value) in env::vars() {
		let key = key.into_bytes();
		let value = value.into_bytes();
		envp.insert(key, value);
	}

	for (key, value) in vars {
		let key = key.clone().into_bytes();
		let value = value.clone().into_bytes();

		envp.insert(key, value);
	}

	let mut buf = Vec::<u8>::new();
	let len = envp.len();
	for (key, value) in envp {
		buf.extend_from_slice(&key[..]);
		buf.push('=' as u8);
		buf.extend_from_slice(&value[..]);
		buf.push(0);
	}

	let envp = make_ptr_array(&buf, len);
	(buf, envp)
}

impl Chain {
	/// Send signal `sig` to all child processes.
	pub fn kill(&mut self, sig: c_int) -> Result<()> {
		for child in &self.children {
			if unsafe { kill(child.pid, sig) } == -1 {
				return c_err!();
			}
		}
		Ok(())
	}

	/// Create a new pipe chain. This function is called by the [spawn!] macro.
	pub fn new(pipes: Vec<Pipes>) -> Result<Self> {
		let len = pipes.len();
		if len == 0 {
			return Err(Error::NotEnoughPipes);
		}

		let mut prev_was_pipe = true;
		for pipe in &pipes {
			if pipe.argv.is_empty() {
				return Err(Error::NotEnoughArguments);
			}

			match pipe.stdin {
				PipeSetup::Redirect(target) => {
					return Err(Error::CannotRedirectStdinTo(target));
				},
				PipeSetup::Pipe => {
					if !prev_was_pipe {
						return Err(Error::InvalidPipeLinkup);
					}
				},
				_ => {}
			}

			prev_was_pipe = match pipe.stdout {
				PipeSetup::Pipe | PipeSetup::Redirect(Target::Stdout) => true,
				_ => match pipe.stderr {
					PipeSetup::Redirect(Target::Stdout) => true,
					_ => false
				}
			};
		}

		let mut children = Vec::<ChildIntern>::with_capacity(len);
		let mut index = 0;
		for pipe in pipes {
			let mut child = unsafe { ChildIntern {
				pid: -1,
				infd:  pipe.stdin.into_raw_fd(),
				outfd: pipe.stdout.into_raw_fd(),
				errfd: pipe.stderr.into_raw_fd()
			}};

			if child.outfd == PIPES_TO_STDOUT {
				child.outfd = PIPES_PIPE;
			}

			if child.errfd == PIPES_TO_STDERR {
				child.errfd = PIPES_PIPE;
			}

			if index > 0 && child.infd == PIPES_PIPE {
				let prevfd = children[index - 1].outfd;
				if prevfd >= 0 {
					child.infd = prevfd;
					children[index - 1].outfd = -1;
				} else {
					child.infd = PIPES_INHERIT;
				}
			}

			let (argvbuf, argv) = make_argv(&pipe.argv);
			let (envpbuf, envp) = make_envp(&pipe.envp);

			let res = pipes_open((&argv[..]).as_ptr(), (&envp[..]).as_ptr(), &mut child, pipe.last);

			// I hope these explicit drops ensure the lifetime of the buffers
			// until this point, even with non-lexical lifetimes.
			drop(argvbuf);
			drop(envpbuf);

			if res.is_err() {
				for mut child in &mut children {
					unsafe { kill(child.pid, SIGTERM); }
					pipes_close(&mut child);
				}
				res?;
			}

			children.push(child);
			index += 1;
		}

		Ok(Chain { children })
	}

	/// Take ownership of stdin of the first process in the chain.
	pub fn stdin(&mut self) -> Option<File> {
		let fd = self.children[0].infd;
		if fd < 0 {
			return None;
		}
		self.children[0].infd = -1;
		Some(unsafe { File::from_raw_fd(fd) })
	}

	/// Take ownership of stdout of the last process in the chain.
	pub fn stdout(&mut self) -> Option<File> {
		let index = self.children.len() - 1;
		let fd = self.children[index].outfd;
		if fd < 0 {
			return None;
		}
		self.children[index].outfd = -1;
		Some(unsafe { File::from_raw_fd(fd) })
	}

	/// Take ownership of stderr of the last process in the chain.
	pub fn stderr(&mut self) -> Option<File> {
		let index = self.children.len() - 1;
		let fd = self.children[index].errfd;
		if fd < 0 {
			return None;
		}
		self.children[index].errfd = -1;
		Some(unsafe { File::from_raw_fd(fd) })
	}

	/// Wait for all child processes to finish.
	pub fn wait_all(&mut self) -> Vec<WaitResult> {
		self.children.iter().map(|child| intern_wait(child.pid)).collect()
	}

	/// Wait for the last child process to finish.
	pub fn wait_last(&mut self) -> WaitResult {
		let index = self.children.len() - 1;
		intern_wait(self.children[index].pid)
	}

	/// Close stdin of the first child process and read stdout and stderr
	/// of the last child process (if possible) and wait for the last
	/// child process to finish.
	pub fn output(mut self) -> result::Result<Output, OutputError> {
		drop(self.stdin());

		let mut stdout = Vec::new();
		let mut stderr = Vec::new();
		match (self.stdout(), self.stderr()) {
			(None, None) => {},
			(Some(mut out), None) => {
				match out.read_to_end(&mut stdout) {
					Ok(_) => {},
					Err(err) => {
						return Err(OutputError::IO(err));
					}
				}
			},
			(None, Some(mut err)) => {
				match err.read_to_end(&mut stdout) {
					Ok(_) => {},
					Err(err) => {
						return Err(OutputError::IO(err));
					}
				}
			},
			(Some(mut out), Some(mut err)) => {
				let mut buf = [0u8; BUFSIZ as usize];
				let mut pollfds = [
					pollfd {
						fd: out.as_raw_fd(),
						events: POLLIN,
						revents: 0
					},
					pollfd {
						fd: err.as_raw_fd(),
						events: POLLIN,
						revents: 0
					}
				];

				while pollfds[0].events != 0 && pollfds[1].events != 0 {
					let res = unsafe { poll(pollfds.as_mut_ptr(), 2, -1) };

					if res < 0 {
						return Err(OutputError::Pipe(Error::OS(unsafe { *__errno_location() })));
					}

					if pollfds[0].revents & POLLIN != 0 {
						match out.read(&mut buf) {
							Ok(size) => {
								stdout.extend_from_slice(&mut buf[..size]);
							},
							Err(err) => {
								return Err(OutputError::IO(err));
							}
						}
					}

					if pollfds[0].revents & (POLLERR | POLLHUP) != 0 {
						pollfds[0].fd = -1;
						pollfds[0].events = 0;
					}

					if pollfds[0].revents & POLLNVAL != 0 {
						return Err(OutputError::Pipe(Error::OS(unsafe { *__errno_location() })));
					}

					if pollfds[1].revents & POLLIN != 0 {
						match err.read(&mut buf) {
							Ok(size) => {
								stderr.extend_from_slice(&mut buf[..size]);
							},
							Err(err) => {
								return Err(OutputError::IO(err));
							}
						}
					}
					
					if pollfds[1].revents & (POLLERR | POLLHUP) != 0 {
						pollfds[1].fd = -1;
						pollfds[1].events = 0;
					}

					if pollfds[1].revents & POLLNVAL != 0 {
						return Err(OutputError::Pipe(Error::OS(unsafe { *__errno_location() })));
					}

					pollfds[0].revents = 0;
					pollfds[1].revents = 0;
				}
			}
		}

		match self.wait_last() {
			Err(err) => Err(OutputError::Wait(err)),
			Ok(status) => Ok(Output {
				status,
				stdout,
				stderr,
			})
		}
	}
}

impl Drop for Chain {
	fn drop(&mut self) {
		for mut child in &mut self.children {
			pipes_close(child);
		}
	}
}
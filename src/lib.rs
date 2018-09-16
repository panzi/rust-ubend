//! # UBend
//!
//! This is a small crate that lets you build pipe chains between spawned
//! processes using a syntax similar to the Unix shell.
//!
//! ```
//! # #[macro_use] extern crate ubend;
//! # use ubend::IntoPipeSetup;
//! 
//! let output = ubend!(
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
//! `use ubend::IntoPipeSetup` is needed when passing a file name or
//! `std::fs::File` as redirection source/target.
//! 
//! **Note:** Currently only Linux ist tested. Other Unix operating systems might
//! work, too. Windows support is not implemented.
//! 
//! ## More Examples
//! 
//! ```
//! # #[macro_use] extern crate ubend;
//! # use ubend::IntoPipeSetup;
//! # use std::fs::File;
//! # use std::io::{Read, Seek, SeekFrom};
//! use ubend::PipeSetup::*;
//! 
//! // Ignore stderr
//! ubend!(rm "no_such_file" 2>Null);
//! 
//! // Redirect stderr to stdout
//! ubend!(rm "no_such_file" 2>&1);
//! 
//! // Write stderr to stderr of this process
//! ubend!(rm "no_such_file" 2>Inherit);
//! 
//! // Read from a file opened in Rust
//! let file = File::open("./tests/input.txt").
//! 	expect("couldn't open file");
//! ubend!(grep "spam" <file);
//! 
//! // Write stderr to a temp file
//! let mut chain = ubend!(rm "no_such_file" 2>Temp).
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
	pipe2, // Linux & FreeBSD (macOS?)
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
	O_CLOEXEC,
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
	EPERM,
	ESRCH,
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
	POLLNVAL,
	fcntl,
	F_GETFD,
	F_SETFD,
	FD_CLOEXEC
};

const UBEND_INHERIT:   c_int = -2;
const UBEND_PIPE:      c_int = -3;
const UBEND_NULL:      c_int = -4;
const UBEND_TO_STDOUT: c_int = -5;
const UBEND_TO_STDERR: c_int = -6;
const UBEND_TEMP:      c_int = -7;

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
			PipeSetup::Inherit => UBEND_INHERIT,
			PipeSetup::Pipe => UBEND_PIPE,
			PipeSetup::Null => UBEND_NULL,
			PipeSetup::Redirect(Target::Stdout) => UBEND_TO_STDOUT,
			PipeSetup::Redirect(Target::Stderr) => UBEND_TO_STDERR,
			PipeSetup::Temp => UBEND_TEMP,
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
pub struct Command {
	pub stdin:  PipeSetup,
	pub stdout: PipeSetup,
	pub stderr: PipeSetup,
	pub last: Target,
	pub argv: Vec<String>,
	pub envp: HashMap<String, String>
}

#[derive(Debug)]
pub struct Child {
	pid:   pid_t,
	infd:  c_int,
	outfd: c_int,
	errfd: c_int,
}

/// Represents a pipe chain.
#[derive(Debug)]
pub struct Chain {
	children: Vec<Child>
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

pub enum KillError {
	InvalidSignal,
	NoPermissions,
	InvalidProcess
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
		}
	}
}

impl Display for WaitError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
		match self {
			WaitError::Interrupted => f.write_str("wait was interrupted"),
			WaitError::ChildInvalid => f.write_str("process does not exist or is not a child of calling process"),
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

impl Display for KillError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
		match self {
			KillError::InvalidSignal => f.write_str("invalid signal"),
			KillError::NoPermissions => f.write_str("no permissions to send signal to process"),
			KillError::InvalidProcess => f.write_str("no such process")
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

impl std::fmt::Debug for KillError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
		(self as &Display).fmt(f)
	}
}

#[macro_export]
#[doc(hidden)]
macro_rules! ubend_unexpected_end {
	($tt:tt) => {}
}

#[macro_export]
#[doc(hidden)]
macro_rules! ubend_unexpected_token {
	() => {}
}

#[macro_export]
#[doc(hidden)]
macro_rules! ubend_illegal_mode {
	() => {}
}

#[macro_export]
#[doc(hidden)]
macro_rules! ubend_internal_envp {
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
macro_rules! ubend_internal {
	(stdin ($($stream:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($last:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($cont:tt)*) ($($chain:tt)*)) => {
		ubend_internal!($($cont)* (($($stream)*) ($($stdout)*) ($($stderr)*) ($($last)*) ($($argv)*) ($($envp)*)) ($($chain)*))
	};

	(stdout ($($stream:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($last:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($cont:tt)*) ($($chain:tt)*)) => {
		ubend_internal!($($cont)* (($($stdin)*) ($($stream)*) ($($stderr)*) (ubend::Target::Stdout) ($($argv)*) ($($envp)*)) ($($chain)*))
	};

	(stderr ($($stream:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($last:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($cont:tt)*) ($($chain:tt)*)) => {
		ubend_internal!($($cont)* (($($stdin)*) ($($stdout)*) ($($stream)*) (ubend::Target::Stderr) ($($argv)*) ($($envp)*)) ($($chain)*))
	};

	(@arg ($($arg:tt)*) ($($rest:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($last:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($chain:tt)*)) => {
		ubend_internal!(@body () ($($rest)*) (($($stdin)*) ($($stdout)*) ($($stderr)*) ($($last)*) ($($argv)* ($($arg)*)) ($($envp)*)) ($($chain)*))
	};

	(@var ($($key:tt)*) ($($val:tt)*) ($($rest:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($last:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($chain:tt)*)) => {
		ubend_internal!(@head () ($($rest)*) (($($stdin)*) ($($stdout)*) ($($stderr)*) ($($last)*) ($($argv)*) ($($envp)* (($($key)*) ($($val)*)))) ($($chain)*))
	};

	// ========== ENVIRONMENT VARIABLES ========================================
	(@env ($key:ident) ($val:expr) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@var (stringify!($key)) ($val) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@env ($key:ident) ($($tt:tt)*) ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@env ($key) ($($tt)* $tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	// ========== HEAD =========================================================
	(@head ($id:ident) (= $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@env ($id) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@head ($id:ident) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@arg (stringify!($id)) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@head ($id:expr) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@arg ($id) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@head () ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@head ($tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@head ($tt:tt) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_unexpected_token!($tt)
	};

	// ========== HELPER =======================================================
	(@end ($($chain:tt)*)) => {
		vec![$($chain),*]
	};

	(@chain ($($elem:tt)*) ($($chain:tt)*) ($($cont:tt)*)) => {
		ubend_internal!($($cont)* ($($chain)* ($($elem)*)))
	};

	// ========== BODY =========================================================
	(@body () () (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($last:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($chain:tt)*)) => {
		ubend_internal!(@chain (ubend::Command {
			stdin: $($stdin)*,
			stdout: $($stdout)*,
			stderr: $($stderr)*,
			last: $($last)*,
			argv: vec![$($argv.to_string()),*],
			envp: ubend_internal_envp!($($envp)*)
		}) ($($chain)*) (@end))
	};

	(@body (|) () ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_unexpected_end!()
	};

	(@body (|) (| $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_unexpected_token!(|)
	};

	(@body (|) ($($rest:tt)+) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($last:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($chain:tt)*)) => {
		ubend_internal!(@chain
			(ubend::Command {
				stdin: $($stdin)*,
				stdout: $($stdout)*,
				stderr: $($stderr)*,
				last: $($last)*,
				argv: vec![$($argv.to_string()),*],
				envp: ubend_internal_envp!($($envp)*)
			})
			($($chain)*)
			(@head () ($($rest)+) ((ubend::PipeSetup::Pipe) (ubend::PipeSetup::Pipe) (ubend::PipeSetup::Inherit) (ubend::Target::Stderr) () ()))
		)
	};

	(@body (<) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@pipe stdin (ubend::Mode::Read) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body (0) (< $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@pipe stdin (ubend::Mode::Read) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body () (>> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@pipe stdout (ubend::Mode::Append) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body (1) (>> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@pipe stdout (ubend::Mode::Append) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body (2) (>> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@pipe stderr (ubend::Mode::Append) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body () (> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@pipe stdout (ubend::Mode::Write) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body (1) (> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@pipe stdout (ubend::Mode::Write) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body (2) (> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@pipe stderr (ubend::Mode::Write) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body ($arg:expr) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@arg ($arg) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body ($($tt:tt)*) ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@body ($($tt)* $tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	// ========== PIPE =========================================================
	(@pipe $pipe:ident (ubend::Mode::Write) (&) (1 $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!($pipe (ubend::PipeSetup::Redirect(ubend::Target::Stdout)) ($($opts)*) (@body () ($($rest)*)) ($($chain)*))
	};

	(@pipe $pipe:ident (ubend::Mode::Write) (&) (2 $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!($pipe (ubend::PipeSetup::Redirect(ubend::Target::Stderr)) ($($opts)*) (@body () ($($rest)*)) ($($chain)*))
	};

	(@pipe $pipe:ident (ubend::Mode::Write) (&) ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_unexpected_token!($tok)
	};

	(@pipe $pipe:ident ($($mode:tt)*) (&) ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_illegal_mode!($mode)
	};

	(@pipe $pipe:ident ($($mode:tt)*) ($($tt:tt)*) (:: $tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@pipe $pipe ($($mode)*) ($($tt)* :: $tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@pipe $pipe:ident ($($mode:tt)*) ($($tt:tt)*) (. $tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@pipe $pipe ($($mode)*) ($($tt)* . $tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@pipe $pipe:ident ($($mode:tt)*) ($io:expr) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!($pipe ($io.into_pipe_setup($($mode)*)) ($($opts)*) (@body () ($($rest)*)) ($($chain)*))
	};

	(@pipe $pipe:ident ($($mode:tt)*) ($($tt:tt)*) ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@pipe $pipe ($($mode)*) ($($tt)* $tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};
}

/// Create a pipe chain using a Unix shell like syntax.
#[macro_export]
macro_rules! ubend {
	($($tt:tt)*) => {
		ubend::Chain::new(
			// arguments: @marker (current token) (tokens to parse) ((stdin) (stdout) (stderr) (last) (argv) (envp)) (pipe chain array)
			ubend_internal!(@head () ($($tt)*) ((ubend::PipeSetup::Inherit) (ubend::PipeSetup::Pipe) (ubend::PipeSetup::Inherit) (ubend::Target::Stderr) () ()) ()))
	}
}

fn open_temp_fd_fallback() -> c_int {
	let mut name: [u8; 22] = *b"/tmp/rust-ubendXXXXXX\0";
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
	unsafe {
		if oldfd > -1 {
			if oldfd == newfd {
				// In the (unlikely) case the new file descriptor is the same
				// as the old file descriptor we still need to make sure the
				// close on exec flag is NOT set.
				//
				// This can only happen if the calling process closed all its
				// standard IO streams (thus making them available) and thus
				// the pipe2() call happened to yield the needed target file
				// descriptor just by chance.
				let flags = fcntl(oldfd, F_GETFD);

				if flags == -1 {
					perror(errmsg);
					exit(EXIT_FAILURE);
				}

				if flags & FD_CLOEXEC != 0 {
					let flags = flags & !FD_CLOEXEC;

					if fcntl(oldfd, F_SETFD, flags) != 0 {
						perror(errmsg);
						exit(EXIT_FAILURE);
					}
				}
			} else {
				if dup2(oldfd, newfd) == -1 {
					perror(errmsg);
					exit(EXIT_FAILURE);
				}
				close(oldfd);
			}
		}
	}
}

fn redirect_out_fd(action: c_int, oldfd: c_int, newfd: c_int, errmsg: *const c_char) {
	match action {
		::UBEND_TO_STDOUT => redirect_fd(STDOUT_FILENO, newfd, errmsg),
		::UBEND_TO_STDERR => redirect_fd(STDERR_FILENO, newfd, errmsg),
		_                 => redirect_fd(oldfd, newfd, errmsg),
	}
}

impl Child {
	pub fn kill(&mut self, sig: c_int) -> result::Result<(), KillError> {
		if unsafe { kill(self.pid, sig) } == -1 {
			return match unsafe { *__errno_location() } {
				::EINVAL => Err(KillError::InvalidSignal),
				::EPERM  => Err(KillError::NoPermissions),
				::ESRCH  => Err(KillError::InvalidProcess),
				errno => panic!("unhandeled errno: {}", errno)
			}
		}
		Ok(())
	}

	pub fn wait(&mut self) -> WaitResult {
		if self.pid <= 0 {
			return Err(WaitError::ChildInvalid);
		}

		let mut status: c_int = -1;
		if unsafe { waitpid(self.pid, &mut status, 0) } == -1 {
			return Err(match unsafe { *__errno_location() } {
				::ECHILD => WaitError::ChildInvalid,
				::EINTR => WaitError::Interrupted,
				errno => panic!("unhandled errno: {}", errno)
			});
		}

		self.pid = -1;

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

	pub fn pid(&self) -> pid_t {
		self.pid
	}

	/// Take ownership of stdin of the process.
	///
	/// If the setup didn't create a pipe for stdin or the pipe was already
	// taken this function returns None.
	pub fn stdin(&mut self) -> Option<File> {
		let fd = self.infd;
		if fd < 0 {
			return None;
		}
		self.infd = -1;
		Some(unsafe { File::from_raw_fd(fd) })
	}

	/// Take ownership of stdout of the process.
	///
	/// If the setup didn't create a pipe for stdout or the pipe was already
	// taken this function returns None.
	pub fn stdout(&mut self) -> Option<File> {
		let fd = self.outfd;
		if fd < 0 {
			return None;
		}
		self.outfd = -1;
		Some(unsafe { File::from_raw_fd(fd) })
	}

	/// Take ownership of stderr of the process.
	///
	/// If the setup didn't create a pipe for stderr or the pipe was already
	// taken this function returns None.
	pub fn stderr(&mut self) -> Option<File> {
		let fd = self.errfd;
		if fd < 0 {
			return None;
		}
		self.errfd = -1;
		Some(unsafe { File::from_raw_fd(fd) })
	}

	/// Close stdin of the child process, read its stdout and stderr (if
	/// possible) and wait for the child process to finish.
	pub fn output(mut self) -> result::Result<Output, OutputError> {
		drop(self.stdin());
		self.internal_output()
	}

	fn internal_output(&mut self) -> result::Result<Output, OutputError> {
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

		match self.wait() {
			Err(err) => Err(OutputError::Wait(err)),
			Ok(status) => Ok(Output {
				status,
				stdout,
				stderr,
			})
		}
	}
}

impl Drop for Child {
	fn drop(&mut self) {
		unsafe {
			if self.infd > -1 {
				close(self.infd);
				self.infd = -1;
			}

			if self.outfd > -1 {
				close(self.outfd);
				self.outfd = -1;
			}

			if self.errfd > -1 {
				close(self.errfd);
				self.errfd = -1;
			}
		}
	}
}

fn handle_error(infd: c_int, outfd: c_int, errfd: c_int) -> Result<()> {
	unsafe {
		let errno = *__errno_location();

		if infd  > -1 { close(infd); }
		if outfd > -1 { close(outfd); }
		if errfd > -1 { close(errfd); }

		return Err(Error::OS(errno));
	}
}

fn ubend_open(argv: *const *const c_char, envp: *const *const c_char, child: &mut Child, last: Target) -> Result<()> {
	unsafe {
		let mut infd  = -1;
		let mut outfd = -1;
		let mut errfd = -1;

		let inaction  = child.infd;
		let outaction = child.outfd;
		let erraction = child.errfd;

		match inaction {
			::UBEND_PIPE => {
				let mut pair: [c_int; 2] = [-1, -1];
				if pipe2(pair.as_mut_ptr(), O_CLOEXEC) == -1 {
					return handle_error(infd, outfd, errfd);
				}
				infd = pair[0];
				child.infd = pair[1];
			},
			::UBEND_NULL => {
				infd = open(cstr!(b"/dev/null\0"), O_RDONLY);

				if infd < 0 {
					return handle_error(infd, outfd, errfd);
				}
			},
			::UBEND_TEMP => {
				infd = open_temp_fd();
				child.infd = infd;

				if infd < 0 {
					return handle_error(infd, outfd, errfd);
				}
			},
			::UBEND_INHERIT => {},
			_ if inaction > -1 => {
				infd = inaction;
				child.infd = -1;
			},
			_ => {
				*__errno_location() = EINVAL;
				return handle_error(infd, outfd, errfd);
			}
		}

		match outaction {
			::UBEND_PIPE => {
				let mut pair: [c_int; 2] = [-1, -1];
				if pipe2(pair.as_mut_ptr(), O_CLOEXEC) == -1 {
					return handle_error(infd, outfd, errfd);
				}
				outfd = pair[1];
				child.outfd = pair[0];
			},
			::UBEND_NULL => {
				outfd = open(cstr!(b"/dev/null\0"), O_WRONLY);

				if outfd < 0 {
					return handle_error(infd, outfd, errfd);
				}
			},
			::UBEND_TO_STDERR => {},
			::UBEND_TEMP => {
				outfd = open_temp_fd();
				child.outfd = outfd;

				if outfd < 0 {
					return handle_error(infd, outfd, errfd);
				}
			},
			::UBEND_INHERIT => {},
			_ if outaction > -1 => {
				outfd = outaction;
				child.outfd = -1;
			},
			_ => {
				*__errno_location() = EINVAL;
				return handle_error(infd, outfd, errfd);
			}
		}

		match erraction {
			::UBEND_PIPE => {
				let mut pair: [c_int; 2] = [-1, -1];
				if pipe2(pair.as_mut_ptr(), O_CLOEXEC) == -1 {
					return handle_error(infd, outfd, errfd);
				}
				errfd = pair[1];
				child.errfd = pair[0];
			},
			::UBEND_NULL => {
				errfd = open(cstr!(b"/dev/null\0"), O_WRONLY);

				if errfd < 0 {
					return handle_error(infd, outfd, errfd);
				}
			},
			::UBEND_TO_STDOUT => {},
			::UBEND_TEMP => {
				errfd = open_temp_fd();
				child.errfd = errfd;

				if errfd < 0 {
					return handle_error(infd, outfd, errfd);
				}
			},
			::UBEND_INHERIT => {},
			_ if erraction > -1 => {
				errfd = erraction;
				child.errfd = -1;
			},
			_ => {
				*__errno_location() = EINVAL;
				return handle_error(infd, outfd, errfd);
			}
		}

		let pid = fork();

		if pid == -1 {
			return handle_error(infd, outfd, errfd);
		}

		if pid == 0 {
			// child

			// close unused ends
			if child.infd > -1 && child.infd != infd {
				close(child.infd);
				child.infd = -1;
			}

			if child.outfd > -1 && child.outfd != outfd {
				close(child.outfd);
				child.outfd = -1;
			}

			if child.errfd > -1 && child.errfd != errfd {
				close(child.errfd);
				child.errfd = -1;
			}

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

			if inaction  != UBEND_TEMP && infd  > -1 { close(infd); }
			if outaction != UBEND_TEMP && outfd > -1 { close(outfd); }
			if erraction != UBEND_TEMP && errfd > -1 { close(errfd); }
		}

		Ok(())
	}
}

impl Command {
	pub fn empty() -> Self {
		Command {
			stdin:  PipeSetup::Inherit,
			stdout: PipeSetup::Inherit,
			stderr: PipeSetup::Inherit,
			last: Target::Stderr,
			argv: Vec::with_capacity(0),
			envp: HashMap::with_capacity(0)
		}
	}

	pub fn new(prog: &str) -> Self {
		Command {
			stdin:  PipeSetup::Inherit,
			stdout: PipeSetup::Inherit,
			stderr: PipeSetup::Inherit,
			last: Target::Stderr,
			argv: vec![prog.to_string()],
			envp: HashMap::new()
		}
	}

	pub fn first(args: &[&str]) -> Self {
		Command {
			stdin:  PipeSetup::Inherit,
			stdout: PipeSetup::Pipe,
			stderr: PipeSetup::Inherit,
			last: Target::Stderr,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: HashMap::new()
		}
	}

	pub fn last(args: &[&str]) -> Self {
		Command {
			stdin:  PipeSetup::Pipe,
			stdout: PipeSetup::Inherit,
			stderr: PipeSetup::Inherit,
			last: Target::Stderr,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: HashMap::new()
		}
	}

	pub fn pass_through(args: &[&str]) -> Self {
		Command {
			stdin:  PipeSetup::Pipe,
			stdout: PipeSetup::Pipe,
			stderr: PipeSetup::Inherit,
			last: Target::Stderr,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: HashMap::new()
		}
	}

	pub fn pass_stdin(fd: c_int, args: &[&str]) -> Self {
		Command {
			stdin:  PipeSetup::FileDescr(fd),
			stdout: PipeSetup::Pipe,
			stderr: PipeSetup::Inherit,
			last: Target::Stderr,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: HashMap::new()
		}
	}

	pub fn pass_stdout(fd: c_int, args: &[&str]) -> Self {
		Command {
			stdin:  PipeSetup::Pipe,
			stdout: PipeSetup::FileDescr(fd),
			stderr: PipeSetup::Inherit,
			last: Target::Stderr,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: HashMap::new()
		}
	}

	pub fn pass_stderr(fd: c_int, args: &[&str]) -> Self {
		Command {
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

	pub fn open(self) -> Result<Child> {
		if self.argv.is_empty() {
			return Err(Error::NotEnoughArguments);
		}

		match self.stdin {
			PipeSetup::Redirect(target) => {
				return Err(Error::CannotRedirectStdinTo(target));
			},
			_ => {}
		}

		let mut child = unsafe { Child {
			pid: -1,
			infd:  self.stdin.into_raw_fd(),
			outfd: self.stdout.into_raw_fd(),
			errfd: self.stderr.into_raw_fd()
		}};

		if child.outfd == UBEND_TO_STDOUT {
			child.outfd = UBEND_PIPE;
		}

		if child.errfd == UBEND_TO_STDERR {
			child.errfd = UBEND_PIPE;
		}

		let (argvbuf, argv) = make_argv(&self.argv);
		let (envpbuf, envp) = make_envp(&self.envp);

		ubend_open((&argv).as_ptr(), (&envp).as_ptr(), &mut child, self.last)?;

		// I hope these explicit drops ensure the lifetime of the buffers
		// until this point, even with non-lexical lifetimes.
		drop(argvbuf);
		drop(envpbuf);

		Ok(child)
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

fn make_argv<Str: AsRef<str>>(argv: &[Str]) -> (Vec<u8>, Vec<*const c_char>) {
	let mut buf = Vec::<u8>::new();
	for arg in argv {
		buf.extend_from_slice(arg.as_ref().as_bytes());
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
		buf.extend_from_slice(&key);
		buf.push('=' as u8);
		buf.extend_from_slice(&value);
		buf.push(0);
	}

	let envp = make_ptr_array(&buf, len);
	(buf, envp)
}

impl Chain {
	/// Send signal `sig` to all child processes.
	pub fn kill_all(&mut self, sig: c_int) -> result::Result<(), KillError> {
		for child in &mut self.children {
			child.kill(sig)?;
		}
		Ok(())
	}

	/// Create a new pipe chain. This function is called by the [ubend!] macro.
	pub fn new(pipes: Vec<Command>) -> Result<Self> {
		let len = pipes.len();
		if len == 0 {
			return Err(Error::NotEnoughPipes);
		}

		for pipe in &pipes {
			if pipe.argv.is_empty() {
				return Err(Error::NotEnoughArguments);
			}

			match pipe.stdin {
				PipeSetup::Redirect(target) => {
					return Err(Error::CannotRedirectStdinTo(target));
				},
				_ => {}
			}
		}

		let mut children = Vec::<Child>::with_capacity(len);
		let mut index = 0;
		for pipe in pipes {
			let mut child = unsafe { Child {
				pid: -1,
				infd:  pipe.stdin.into_raw_fd(),
				outfd: pipe.stdout.into_raw_fd(),
				errfd: pipe.stderr.into_raw_fd()
			}};

			if child.outfd == UBEND_TO_STDOUT {
				child.outfd = UBEND_PIPE;
			}

			if child.errfd == UBEND_TO_STDERR {
				child.errfd = UBEND_PIPE;
			}

			if index > 0 && child.infd == UBEND_PIPE {
				let prevfd = children[index - 1].outfd;
				if prevfd > -1 {
					child.infd = prevfd;
					children[index - 1].outfd = -1;
				}
				// Otherwise chain.stdin_at(index) has to be called by the API
				// user in order to provide that particular stream with data.
			}

			let (argvbuf, argv) = make_argv(&pipe.argv);
			let (envpbuf, envp) = make_envp(&pipe.envp);

			let res = ubend_open((&argv).as_ptr(), (&envp).as_ptr(), &mut child, pipe.last);

			// I hope these explicit drops ensure the lifetime of the buffers
			// until this point, even with non-lexical lifetimes.
			drop(argvbuf);
			drop(envpbuf);

			match res {
				Err(err) => {
					for mut child in &mut children {
						unsafe { kill(child.pid, SIGTERM); }
					}
					return Err(err);
				}
				_ => {}
			}

			children.push(child);
			index += 1;
		}

		Ok(Chain { children })
	}

	/// Take ownership of stdin of the first process in the chain.
	///
	/// If the setup didn't create a pipe at the given location or the pipe was
	/// already taken this function returns None.
	pub fn stdin(&mut self) -> Option<File> {
		self.stdin_at(0)
	}

	/// Take ownership of stdout of the last process in the chain.
	///
	/// If the setup didn't create a pipe at the given location or the pipe was
	/// already taken this function returns None.
	pub fn stdout(&mut self) -> Option<File> {
		let index = self.children.len() - 1;
		self.stdout_at(index)
	}

	/// Take ownership of stderr of the last process in the chain.
	///
	/// If the setup didn't create a pipe at the given location or the pipe was
	/// already taken this function returns None.
	pub fn stderr(&mut self) -> Option<File> {
		let index = self.children.len() - 1;
		self.stderr_at(index)
	}

	/// Take ownership of stdin of the process at index in the chain.
	///
	/// If the index is out of bounds or the setup didn't create a pipe at the
	/// given location or the pipe was already taken this function returns None.
	pub fn stdin_at(&mut self, index: usize) -> Option<File> {
		if let Some(child) = self.children.get_mut(index) {
			return child.stdin();
		}
		None
	}

	/// Take ownership of stdout of the process at index in the chain.
	///
	/// If the index is out of bounds or the setup didn't create a pipe at the
	/// given location or the pipe was already taken this function returns None.
	pub fn stdout_at(&mut self, index: usize) -> Option<File> {
		if let Some(child) = self.children.get_mut(index) {
			return child.stdout();
		}
		None
	}

	/// Take ownership of stderr of the process at index in the chain.
	///
	/// If the index is out of bounds or the setup didn't create a pipe at the
	/// given location or the pipe was already taken this function returns None.
	pub fn stderr_at(&mut self, index: usize) -> Option<File> {
		if let Some(child) = self.children.get_mut(index) {
			return child.stderr();
		}
		None
	}

	pub fn children(&self) -> &[Child] {
		&self.children
	}

	pub fn children_mut(&mut self) -> &mut [Child] {
		&mut self.children
	}

	/// Wait for all child processes to finish.
	pub fn wait_all(&mut self) -> Vec<WaitResult> {
		self.children.iter_mut().map(|child| child.wait()).collect()
	}

	/// Wait for the last child process to finish.
	pub fn wait_last(&mut self) -> WaitResult {
		let index = self.children.len() - 1;
		self.children[index].wait()
	}

	/// Close stdin of the first child process and read stdout and stderr
	/// of the last child process (if possible) and wait for the last
	/// child process to finish.
	pub fn output(mut self) -> result::Result<Output, OutputError> {
		drop(self.stdin());

		let index = self.children.len() - 1;
		self.children[index].internal_output()
	}
}
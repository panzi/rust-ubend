//! # U-Bend
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
use std::fs::{File};
use std::ptr;
use std::env;
use std::mem::{drop};
use std::io::Read;

use libc::{
	fork,
	pipe2, // Linux & FreeBSD (macOS?)
	strerror_r, // XSI
	perror,
	open,
	close,
	dup,
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
	EFAULT,
	EACCES,
	EEXIST,
	ENOTDIR,
	EBADF,
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

/// Redirection target. Used with [PipeSetup](enum.PipeSetup.html).
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Target {
	Stdout,
	Stderr
}

/// File open mode. Used with [PipeSetup](enum.PipeSetup.html).
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
	/// is redirected to itself it is equivalent to [PipeSetup::Pipe](enum.PipeSetup.html#variant.Pipe).
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

	/// Connect the stream to the specified file using the specified mode.
	FileName(String, Mode),

	/// Connect the stream to the specified file. Note that error or not any
	/// passed file will be consumed (closed) by [Chain::new()](struct.Chain.html#method.new).
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
			PipeSetup::File(_) | PipeSetup::FileName(_, _) => true,
			_ => false
		}
	}

	pub fn is_file(&self) -> bool {
		match self {
			PipeSetup::File(_) => true,
			_ => false
		}
	}

	pub fn is_file_name(&self) -> bool {
		match self {
			PipeSetup::FileName(_, _) => true,
			_ => false
		}
	}
}

#[derive(Debug)]
enum InternalSetup {
	Inherit,
	InPipe,
	Pipe(c_int, c_int),
	File(c_int),
	Temp(c_int)
}

impl InternalSetup {
	fn pipe() -> std::io::Result<Self> {
		let mut pair: [c_int; 2] = [-1, -1];
		unsafe {
			if pipe2(pair.as_mut_ptr(), O_CLOEXEC) == -1 {
				let errno = *__errno_location();
				let kind = match errno {
					::EFAULT => std::io::ErrorKind::InvalidInput,
					::EINVAL => std::io::ErrorKind::InvalidInput,
					_        => std::io::ErrorKind::Other
				};
				return Err(std::io::Error::new(kind, strerror(errno)));
			}
		}
		Ok(InternalSetup::Pipe(pair[0], pair[1]))
	}

	fn null(mode: Mode) -> std::io::Result<Self> {
		InternalSetup::open("/dev/null", mode)
	}

	fn open(name: &str, mode: Mode) -> std::io::Result<Self> {
		unsafe {
			let fd = match mode {
				Mode::Read   => open(cstr!(name), O_RDONLY),
				Mode::Write  => open(cstr!(name), O_WRONLY | O_CREAT | O_TRUNC,  S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH),
				Mode::Append => open(cstr!(name), O_WRONLY | O_CREAT | O_APPEND, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH),
			};
			if fd < 0 {
				let errno = *__errno_location();
				let kind = match errno {
					::ENOENT  => std::io::ErrorKind::NotFound,
					::EACCES  => std::io::ErrorKind::PermissionDenied,
					::EPERM   => std::io::ErrorKind::PermissionDenied,
					::EEXIST  => std::io::ErrorKind::AlreadyExists,
					::EINTR   => std::io::ErrorKind::Interrupted,
					::EINVAL  => std::io::ErrorKind::InvalidInput,
					::EISDIR  => std::io::ErrorKind::InvalidInput,
					::ENOTDIR => std::io::ErrorKind::InvalidInput,
					_         => std::io::ErrorKind::Other
				};
				return Err(std::io::Error::new(kind, strerror(errno)));
			}

			Ok(InternalSetup::File(fd))
		}
	}

	fn temp() -> std::io::Result<Self> {
		unsafe {
			let fd = open_temp_fd();
			if fd < 0 {
				let errno = *__errno_location();
				let kind = match errno {
					::ENOENT  => std::io::ErrorKind::NotFound,
					::EACCES  => std::io::ErrorKind::PermissionDenied,
					::EPERM   => std::io::ErrorKind::PermissionDenied,
					::EEXIST  => std::io::ErrorKind::AlreadyExists,
					::EINTR   => std::io::ErrorKind::Interrupted,
					::EINVAL  => std::io::ErrorKind::InvalidInput,
					::EISDIR  => std::io::ErrorKind::InvalidInput,
					::ENOTDIR => std::io::ErrorKind::InvalidInput,
					_         => std::io::ErrorKind::Other
				};
				return Err(std::io::Error::new(kind, strerror(errno)));
			}

			Ok(InternalSetup::Temp(fd))
		}
	}

	fn dup(fd: c_int) -> std::io::Result<InternalSetup> {
		unsafe {
			let fd2 = dup(fd);
			if fd2 < 0 {
				let errno = *__errno_location();
				let kind = match errno {
					::EINVAL => std::io::ErrorKind::InvalidInput,
					::EBADF  => std::io::ErrorKind::InvalidInput,
					::EINTR  => std::io::ErrorKind::Interrupted,
					_        => std::io::ErrorKind::Other
				};
				return Err(std::io::Error::new(kind, strerror(errno)));
			}
			Ok(InternalSetup::File(fd2))
		}
	}

	fn try_clone(&self) -> std::io::Result<InternalSetup> {
		match self {
			InternalSetup::Inherit => Ok(InternalSetup::Inherit),
			InternalSetup::InPipe  => Ok(InternalSetup::InPipe),
			InternalSetup::Pipe(_fd1, fd2) => InternalSetup::dup(*fd2),
			InternalSetup::File(fd) => InternalSetup::dup(*fd),
			InternalSetup::Temp(fd) => InternalSetup::dup(*fd),
		}
	}

	/// -> (for_parent, for_child)
	fn as_fd_pair(&self, default_fd: c_int) -> (c_int, c_int) {
		match self {
			InternalSetup::Inherit => (-1, default_fd),
			InternalSetup::InPipe  => (-1, default_fd),
			InternalSetup::Pipe(fd1, fd2) => (*fd1, *fd2),
			InternalSetup::File(fd) => (-1, *fd),
			InternalSetup::Temp(fd) => (*fd, *fd),
		}
	}

	fn drop_as_parent(mut self) {
		match self {
			InternalSetup::Inherit => {},
			InternalSetup::InPipe  => {},
			InternalSetup::Pipe(ref mut parent_fd, _) => {
				*parent_fd = -1;
			},
			InternalSetup::File(_) => {},
			InternalSetup::Temp(ref mut both_fd) => {
				*both_fd = -1;
			},
		}
	}

	#[allow(dead_code)]
	fn drop_as_child(mut self) {
		match self {
			InternalSetup::Inherit => {},
			InternalSetup::InPipe  => {},
			InternalSetup::Pipe(_, ref mut child_fd) => {
				*child_fd = -1;
			},
			InternalSetup::File(ref mut child_fd) => {
				*child_fd = -1;
			},
			InternalSetup::Temp(ref mut both_fd) => {
				*both_fd = -1;
			},
		}
	}

	#[allow(dead_code)]
	fn is_inherit(&self) -> bool {
		match self {
			InternalSetup::Inherit => true,
			_ => false
		}
	}

	#[allow(dead_code)]
	fn is_in_pipe(&self) -> bool {
		match self {
			InternalSetup::InPipe => true,
			_ => false
		}
	}

	#[allow(dead_code)]
	fn is_pipe(&self) -> bool {
		match self {
			InternalSetup::Pipe(_, _) => true,
			_ => false
		}
	}

	#[allow(dead_code)]
	fn is_file(&self) -> bool {
		match self {
			InternalSetup::File(_) => true,
			_ => false
		}
	}

	#[allow(dead_code)]
	fn is_temp(&self) -> bool {
		match self {
			InternalSetup::Temp(_) => true,
			_ => false
		}
	}
}

impl Drop for InternalSetup {
	fn drop(&mut self) {
		match self {
			InternalSetup::Inherit => {},
			InternalSetup::InPipe  => {},
			InternalSetup::Pipe(fd1, fd2) => {
				unsafe {
					if *fd1 > -1 { close(*fd1); }
					if *fd2 > -1 { close(*fd2); }
				}
			},
			InternalSetup::File(fd) => {
				unsafe {
					if *fd > -1 { close(*fd); }
				}
			},
			InternalSetup::Temp(fd) => {
				unsafe {
					if *fd > -1 { close(*fd); }
				}
			}
		}
	}
}

/// Configuration of a child process that is to be started.
#[derive(Debug)]
pub struct Command {
	stdin:  InternalSetup,
	stdout: InternalSetup,
	stderr: InternalSetup,
	argv: Vec<String>,
	envp: HashMap<String, String>
}

/// A running child process.
#[derive(Debug)]
pub struct Child {
	pid:   pid_t,
	infd:  c_int,
	outfd: c_int,
	errfd: c_int,
}

/// A pipe chain of child processes.
#[derive(Debug)]
pub struct Chain {
	children: Vec<Child>
}

/// Errors of opening a pipe chain.
pub enum Error {
	/// A libc function returned an error of specified `errno`.
	///
	/// **Note:** This might change to abstract out the `errno` and unroll the
	/// possible errors into this enum.
	OS(c_int),

	IO(std::io::Error),

	/// Any command needs to have at least one "argument" (the command name).
	NotEnoughArguments,

	/// stdin cannot be redirected to stderr or stdout, since those are output
	/// streams and stdin is an input stream.
	CannotRedirectStdinTo(Target),

	/// A pipe chain has to have at least one command.
	NotEnoughPipes,
}

/// Errors of waiting for a child process to finish.
pub enum WaitError {
	/// Unblocked signal or `SIGCHLD` was caught.
	Interrupted,

	/// No process with given pid exists or it's not a child of the calling process.
	ChildInvalid,

	/// Child process was terminated by given signal.
	/// If the second argument is true the child process' core was dumped.
	ChildSignaled(c_int, bool),

	/// Child process was stopped by given signal.
	ChildStopped(c_int),

	/// Child process was resumed by delivery of `SIGCONT`.
	ChildContinued,
}

/// Errors of getting the output of a child.
pub enum OutputError {
	/// A Rust io function returned the specified error.
	IO(std::io::Error),

	/// A libc function returned an error of specified `errno`.
	///
	/// **Note:** This might change to abstract out the `errno` and unroll the
	/// possible errors into this enum.
	OS(c_int),

	/// Error occured while waiting.
	Wait(WaitError)
}

/// Errors of sening a signal to a child process.
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

/// Trait for redirect targets/sources in [ubend!()](macro.ubend.html) syntax.
///
/// In order for it to be possible to use a String, str or File all the same as
/// a redirection target/source we need a common trait implemented by all of
/// those.
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

fn strerror(errno: c_int) -> String {
	let mut buf = String::new();
	match fmt_os_error(errno, &mut buf) {
		Err(_) => "(error formatting OS error message)".to_string(),
		Ok(_)  => buf
	}
}

fn fmt_os_error(errno: c_int, f: &mut std::fmt::Write) -> std::result::Result<(), std::fmt::Error> {
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
			Error::IO(err) => err.fmt(f),
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
			WaitError::ChildSignaled(sig, dumped) => {
				write!(f, "child exited with signal {}", sig)?;
				if *dumped {
					f.write_str(" and dumped it's core")?;
				}
				Ok(())
			},
			WaitError::ChildStopped(sig) => write!(f, "child stopped with signal {}", sig),
			WaitError::ChildContinued => f.write_str("child continued"),
		}
	}
}

impl Display for OutputError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
		match self {
			OutputError::IO(err)   => err.fmt(f),
			OutputError::OS(errno) => fmt_os_error(*errno, f),
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
macro_rules! ubend_try {
	($($tt:tt)*) => {
		match ($($tt)*) {
			Ok(()) => {},
			Err(err) => break Err(ubend::Error::IO(err)),
		}
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
			let mut __envp = std::collections::HashMap::new();
			$(__envp.insert($($key)*.to_string(), $($val)*.to_string());)*
			__envp
		}
	}
}

#[macro_export]
#[doc(hidden)]
macro_rules! ubend_internal_argv2 {
	($argv:ident ($($code:tt)*)) => {
		$($code)*
	};

	($argv:ident ($($code:tt)*) (@spread $arg:expr) $($rest:tt)*) => {
		ubend_internal_argv2!($argv ({
			$($code)*
			for arg in $arg.iter() {
				($argv).push(arg.to_string());
			}
		}) $($rest)*)
	};

	($argv:ident ($($code:tt)*) ($($arg:tt)*) $($rest:tt)*) => {
		ubend_internal_argv2!($argv ({$($code)* ($argv).push(($($arg)*).to_string());}) $($rest)*)
	}
}

#[macro_export]
#[doc(hidden)]
macro_rules! ubend_internal_argv {
	($($argv:tt)*) => {
		{
			let mut __argv = Vec::<String>::new();
			{ ubend_internal_argv2!(__argv () $($argv)*) }
			__argv
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
		ubend_internal!(@chain ({
			let mut __command = ubend::Command::from_argv_and_envp(
				ubend_internal_argv!($($argv)*),
				ubend_internal_envp!($($envp)*));
			ubend_try!{ __command.stdin($($stdin)*) }
			if $($last)* == ubend::Target::Stderr {
				ubend_try!{ __command.stdout($($stdout)*) }
				ubend_try!{ __command.stderr($($stderr)*) }
			} else {
				ubend_try!{ __command.stdout($($stderr)*) }
				ubend_try!{ __command.stderr($($stdout)*) }
			}
			__command
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
			({
				let mut __command = ubend::Command::from_argv_and_envp(
					ubend_internal_argv!($($argv)*),
					ubend_internal_envp!($($envp)*));
				ubend_try!{ __command.stdin($($stdin)*) }
				if $($last)* == ubend::Target::Stderr {
					ubend_try!{ __command.stdout($($stdout)*) }
					ubend_try!{ __command.stderr($($stderr)*) }
				} else {
					ubend_try!{ __command.stdout($($stderr)*) }
					ubend_try!{ __command.stderr($($stdout)*) }
				}
				__command
			})
			($($chain)*)
			(@head () ($($rest)+) ((ubend::PipeSetup::Pipe) (ubend::PipeSetup::Pipe) (ubend::PipeSetup::Inherit) (ubend::Target::Stderr) () ()))
		)
	};

	(@body ($($arr:tt)*) (... $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		ubend_internal!(@arg (@spread $($arr)*) ($($rest)*) ($($opts)*) ($($chain)*))
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
///
/// This macro returns Result\<[Chain](struct.Chain.html)\>.
///
/// **Note:** In contrast to the Unix shell there is no significant white space
/// in these macros. This means `ubend!(echo "foo">"out.txt")` is the same as
/// `ubend!(echo "foo" > "out.txt")` and similar for all kinds of syntax.
///
/// ```
/// # #[macro_use] extern crate ubend;
/// 
/// // Run a simple command, passing an argument:
/// ubend!(echo "hello world");
/// 
/// // Pass an environment variable:
/// ubend!(LANG="C" gcc "main.c");
/// 
/// // Pass an argument from a variable:
/// let string = "hello world";
/// ubend!(echo string);
/// 
/// // Command from a variable:
/// let command = "echo";
/// ubend!({command} "hello world");
/// 
/// // Arguments from an array/Vec/anything with an `.iter()` method:
/// let args = ["hello", "world"];
/// ubend!(echo args...);
/// 
/// // This can be mixed with other arguments:
/// ubend!(echo "foo" args... string);
/// 
/// // Needed for convenient syntax for redirecting to/from files:
/// use ubend::IntoPipeSetup;
/// 
/// // Write a file by name:
/// ubend!(echo "hello world" >"out.txt");
/// 
/// // Append to a file:
/// ubend!(echo "hello world" >>"out.txt");
/// 
/// // Read from a file:
/// ubend!(cat <"tests/input.txt");
/// 
/// // Use file objects:
/// use std::fs::File;
/// let mut file = File::open("out.txt").expect("open failed");
/// ubend!(echo "hello world" >file);
/// 
/// // Redirecting stderr to stdout:
/// ubend!("./tests/stdio.sh" >&2);
/// 
/// // Multiple redirects work too, of course:
/// ubend!("./tests/stdio.sh" <"tests/input.txt" 2>&1 1>"out.txt");
/// 
/// // Redirect output to a temporary file (the file will be deleted when closed):
/// use ubend::PipeSetup::{*};
/// 
/// let mut chain = ubend!(echo >Temp).expect("spawn failed");
/// chain.wait_last().expect("wait failed");
/// let mut temp = chain.stdout().unwrap();
/// 
/// // You need to reset the stream manually before using:
/// use std::io::{Seek, SeekFrom};
/// temp.seek(SeekFrom::Start(0)).expect("seek failed");
/// 
/// // Rederect from/to `/dev/null`:
/// ubend!(echo "hello world" >Null);
/// 
/// // Explicitely capture a stream:
/// ubend!(echo "hello world" 0<Pipe 1>Pipe 2>Pipe);
/// 
/// // Other way to redirect stdout/stderr to stderr/stdout:
/// use ubend::Target;
/// ubend!(echo "hello world" >(Redirect(Target::Stderr)));
/// ubend!(echo "hello world" 2>(Redirect(Target::Stdout)));
/// // The extra ( ) are neccessary because otherwise it will be parsed as two
/// // arguments `Redirect` and `(Target::Stdout)` by the macro and then will
/// // cause an error in the generated code.
/// 
/// // Make a stream to be inherited from the calling process (instead of
/// // capturing it in a pipe):
/// ubend!(echo "hello world" >Inherit);
/// 
/// // Alternative way to supply a stream from a file by name:
/// use ubend::Mode;
/// ubend!(echo "hello world" >(FileName("out.txt".to_string(), Mode::Append)));
/// 
/// // Alternative way to supply a stream from a file object:
/// let mut file = File::open("tests/input.txt").expect("open failed");
/// ubend!(cat <(ubend::PipeSetup::File(file)));
/// 
/// // Now for the actual multi command piping:
/// ubend!(
/// 	cat <"tests/input.txt" |
/// 	grep "spam" |
/// 	wc "-l"
/// );
/// 
/// ubend!(
/// 	"./tests/stdio.sh" 2>&1 1>Null |
/// 	wc "-c"
/// );
/// 
/// ```
#[macro_export]
macro_rules! ubend {
	($($tt:tt)*) => {
		loop {
			break ubend::Chain::new(
				// arguments: @marker (current token) (tokens to parse) ((stdin) (stdout) (stderr) (last) (argv) (envp)) (pipe chain array)
				ubend_internal!(@head () ($($tt)*)
					((ubend::PipeSetup::Inherit) (ubend::PipeSetup::Pipe) (ubend::PipeSetup::Inherit) (ubend::Target::Stderr)
					() ()) ()));
		}
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

impl Child {
	/// Send a signal to the child process.
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

	/// Wait for the child process to finish and get it's exit status.
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
			return Err(WaitError::ChildSignaled(
				unsafe { WTERMSIG(status) },
				unsafe { WCOREDUMP(status) }));
		}

		if unsafe { WIFSTOPPED(status) } {
			return Err(WaitError::ChildStopped(unsafe { WSTOPSIG(status) }));
		}

		if unsafe { WIFCONTINUED(status) } {
			return Err(WaitError::ChildContinued);
		}

		return Err(WaitError::ChildInvalid);
	}

	/// Get the childs process ID.
	///
	/// If the process already ended and was waited for via
	/// [wait()](struct.Child.html#method.wait) this will return `-1`.
	pub fn pid(&self) -> pid_t {
		self.pid
	}

	/// Take ownership of stdin of the process.
	///
	/// If the setup didn't create a pipe for stdin or the pipe was already
	/// taken this function returns None.
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
	/// taken this function returns None.
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
	/// taken this function returns None.
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
						return Err(OutputError::OS(unsafe { *__errno_location() }));
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
						return Err(OutputError::OS(unsafe { *__errno_location() }));
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
						return Err(OutputError::OS(unsafe { *__errno_location() }));
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

fn ubend_spawn(argv: *const *const c_char, envp: *const *const c_char, stdin: c_int, stdout: c_int, stderr: c_int) -> Result<pid_t> {
	unsafe {
		let pid = fork();

		if pid == -1 {
			return Err(Error::OS(*__errno_location()));
		}

		if pid == 0 {
			// child
			redirect_fd(stdin,  STDIN_FILENO,  cstr!(b"redirecting stdin\0"));
			redirect_fd(stdout, STDOUT_FILENO, cstr!(b"redirecting stdout\0"));
			redirect_fd(stderr, STDERR_FILENO, cstr!(b"redirecting stderr\0"));

			if envp != ptr::null() {
				*environ() = envp;
			}

			if execvp(*argv, argv) == -1 {
				perror(*argv);
			}
			exit(EXIT_FAILURE);
		}
		
		return Ok(pid);
	}
}

fn ubend_sigterm(children: &[Child]) {
	for child in children {
		unsafe { kill(child.pid, SIGTERM); }
	}
}

/*
unsafe fn ubend_close(fd: c_int) -> std::io::Result<()> {
	if close(fd) != 0 {
		let errno = *__errno_location();
		let kind = match errno {
			::EBADF  => std::io::ErrorKind::InvalidInput,
			::EINTR  => std::io::ErrorKind::Interrupted,
			::EINVAL => std::io::ErrorKind::InvalidInput,
			_        => std::io::ErrorKind::Other
		};
		return Err(std::io::Error::new(kind, strerror(errno)));
	}
	Ok(())
}
*/

impl Command {
	/// Helper to create an empty command object inheriting all the streams.
	pub fn empty() -> Self {
		Command {
			stdin:  InternalSetup::Inherit,
			stdout: InternalSetup::Inherit,
			stderr: InternalSetup::Inherit,
			argv: Vec::with_capacity(0),
			envp: HashMap::with_capacity(0)
		}
	}

	/// Helper to create a command object initialized with the program to
	/// execute and inheriting all the streams.
	pub fn new(prog: &str) -> Self {
		Command {
			stdin:  InternalSetup::Inherit,
			stdout: InternalSetup::Inherit,
			stderr: InternalSetup::Inherit,
			argv: vec![prog.to_string()],
			envp: HashMap::new()
		}
	}

	pub fn from_argv_and_envp(argv: Vec<String>, envp: HashMap<String, String>) -> Self {
		Command {
			stdin:  InternalSetup::Inherit,
			stdout: InternalSetup::Inherit,
			stderr: InternalSetup::Inherit,
			argv,
			envp
		}
	}

	/// Helper to create a comand object suitable for the first in a pipe
	/// chain.
	///
	/// `argv` will be initialized by the given args. `stdout` will be
	/// initialized as a pipe, the other streams will inherit.
	pub fn first<Str: AsRef<str>>(args: &[Str]) -> std::io::Result<Self> {
		Ok(Command {
			stdin:  InternalSetup::Inherit,
			stdout: InternalSetup::pipe()?,
			stderr: InternalSetup::Inherit,
			argv: args.iter().map(|arg| arg.as_ref().to_string()).collect(),
			envp: HashMap::new()
		})
	}

	/// Helper to create a comand object suitable for the last in a pipe
	/// chain.
	///
	/// `argv` will be initialized by the given args. `stdin` will be
	/// initialized as a pipe, the other streams will inherit.
	pub fn last<Str: AsRef<str>>(args: &[Str]) -> Self {
		Command {
			stdin:  InternalSetup::InPipe,
			stdout: InternalSetup::Inherit,
			stderr: InternalSetup::Inherit,
			argv: args.iter().map(|arg| arg.as_ref().to_string()).collect(),
			envp: HashMap::new()
		}
	}

	pub fn pass_through<Str: AsRef<str>>(args: &[Str]) -> std::io::Result<Self> {
		Ok(Command {
			stdin:  InternalSetup::InPipe,
			stdout: InternalSetup::pipe()?,
			stderr: InternalSetup::Inherit,
			argv: args.iter().map(|arg| arg.as_ref().to_string()).collect(),
			envp: HashMap::new()
		})
	}

	/// Set an environment variable.
	///
	/// The child process will inherit the environment of this process except
	/// for the variables that are explicitly overwritten.
	pub fn env<Str: AsRef<str>>(&mut self, key: Str, value: Str) {
		self.envp.insert(key.as_ref().to_string(), value.as_ref().to_string());
	}

	/// Add an argument to `argv`.
	pub fn arg<Str: AsRef<str>>(&mut self, arg: Str) {
		self.argv.push(arg.as_ref().to_string());
	}

	/// Setup piping of `stdin`.
	pub fn stdin(&mut self, setup: PipeSetup) -> std::io::Result<()> {
		self.stdin = match setup {
			PipeSetup::Inherit => InternalSetup::Inherit,
			PipeSetup::Pipe => InternalSetup::InPipe,
			PipeSetup::Null => InternalSetup::null(Mode::Read)?,
			PipeSetup::Redirect(_) => {
				return Err(std::io::Error::new(
					std::io::ErrorKind::InvalidInput,
					"cannot redirect stdin to stdout/stderr"));
			},
			PipeSetup::Temp => InternalSetup::temp()?,
			PipeSetup::FileName(name, mode) => {
				if mode != Mode::Read {
					return Err(std::io::Error::new(
						std::io::ErrorKind::InvalidInput,
						"mode must be Read"));
				}
				InternalSetup::open(name.as_str(), mode)?
			},
			PipeSetup::File(file) => InternalSetup::File(file.into_raw_fd()),
		};
		Ok(())
	}

	/// Setup piping of `stdout`.
	///
	/// The order of calling `stdout()` and [stderr()](struct.Command.html#method.stderr)
	/// is relevant when redirecting either to the other. E.g. first redirecting
	/// stdout to stderr and then stderr to a file will write error messages to
	/// the given file and normal output as error messages.
	///
	/// Doing it the other way around will cause both kinds of messages to be
	/// written to the file.
	pub fn stdout(&mut self, setup: PipeSetup) -> std::io::Result<()> {
		self.stdout = match setup {
			PipeSetup::Inherit => InternalSetup::Inherit,
			PipeSetup::Pipe => InternalSetup::pipe()?,
			PipeSetup::Null => InternalSetup::null(Mode::Write)?,
			PipeSetup::Redirect(Target::Stdout) => return Ok(()),
			PipeSetup::Redirect(Target::Stderr) => self.stderr.try_clone()?,
			PipeSetup::Temp => InternalSetup::temp()?,
			PipeSetup::FileName(name, mode) => {
				if mode == Mode::Read {
					return Err(std::io::Error::new(
						std::io::ErrorKind::InvalidInput,
						"mode must not be Read"));
				}
				InternalSetup::open(name.as_str(), mode)?
			},
			PipeSetup::File(file) => InternalSetup::File(file.into_raw_fd()),
		};
		Ok(())
	}

	/// Setup piping of `stderr`.
	///
	/// The order of calling [stdout()](struct.Command.html#method.stdout) and
	/// `stderr()` is relevant when redirecting either to the other. E.g. first
	/// redirecting stdout to stderr and then stderr to a file will write error
	/// messages to the given file and normal output as error messages.
	///
	/// Doing it the other way around will cause both kinds of messages to be
	/// written to the file.
	pub fn stderr(&mut self, setup: PipeSetup) -> std::io::Result<()> {
		self.stderr = match setup {
			PipeSetup::Inherit => InternalSetup::Inherit,
			PipeSetup::Pipe => InternalSetup::pipe()?,
			PipeSetup::Null => InternalSetup::null(Mode::Write)?,
			PipeSetup::Redirect(Target::Stdout) => self.stdout.try_clone()?,
			PipeSetup::Redirect(Target::Stderr) => return Ok(()),
			PipeSetup::Temp => InternalSetup::temp()?,
			PipeSetup::FileName(name, mode) => {
				if mode == Mode::Read {
					return Err(std::io::Error::new(
						std::io::ErrorKind::InvalidInput,
						"mode must not be Read"));
				}
				InternalSetup::open(name.as_str(), mode)?
			},
			PipeSetup::File(file) => InternalSetup::File(file.into_raw_fd()),
		};
		Ok(())
	}

	/// Start a child process.
	pub fn spawn(self) -> Result<Child> {
		if self.argv.is_empty() {
			return Err(Error::NotEnoughArguments);
		}

		let (parent_in,  child_in)  = self.stdin.as_fd_pair(STDIN_FILENO);
		let (parent_out, child_out) = self.stdout.as_fd_pair(STDOUT_FILENO);
		let (parent_err, child_err) = self.stderr.as_fd_pair(STDERR_FILENO);

		let mut child = Child {
			pid: -1,
			infd:  parent_in,
			outfd: parent_out,
			errfd: parent_err
		};

		let (argvbuf, argv) = make_argv(&self.argv);
		let (envpbuf, envp) = make_envp(&self.envp);

		let res = ubend_spawn((&argv).as_ptr(), (&envp).as_ptr(), child_in, child_out, child_err);

		// I hope these explicit drops ensure the lifetime of the buffers
		// until this point, even with non-lexical lifetimes.
		drop(argvbuf);
		drop(envpbuf);

		match res {
			Ok(pid) => {
				child.pid = pid;
				self.drop_as_parent();
			},
			Err(err) => {
				return Err(err);
			}
		}

		Ok(child)
	}

	fn drop_as_parent(self) {
		self.stdin.drop_as_parent();
		self.stdout.drop_as_parent();
		self.stderr.drop_as_parent();
	}

	#[allow(dead_code)]
	fn drop_as_child(self) {
		self.stdin.drop_as_child();
		self.stdout.drop_as_child();
		self.stderr.drop_as_child();
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

	/// Create a new pipe chain, spawning child processes.
	///
	/// This function is called by the [ubend!()](macro.ubend.html) macro.
	pub fn new(commands: Vec<Command>) -> Result<Self> {
		let len = commands.len();
		if len == 0 {
			return Err(Error::NotEnoughPipes);
		}

		for command in &commands {
			if command.argv.is_empty() {
				return Err(Error::NotEnoughArguments);
			}
		}

		let mut children = Vec::<Child>::with_capacity(len);
		let mut index = 0;
		for mut command in commands {
			if index > 0 && command.stdin.is_in_pipe() {
				let mut prev = &mut children[index - 1];
				command.stdin = InternalSetup::File(prev.outfd);
				prev.outfd = -1;
			}

			let mut child = match command.spawn() {
				Ok(child) => child,
				Err(err) => {
					ubend_sigterm(&children);
					return Err(err);
				}
			};

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

	/// The a list of child processes.
	///
	/// This is returned as a slice so that no child can be added/removed,
	/// which is an invariant required by other methods.
	pub fn children(&self) -> &[Child] {
		&self.children
	}

	/// The a mutable list of child processes.
	///
	/// This is returned as a slice so that no child can be added/removed,
	/// which is an invariant required by other methods.
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
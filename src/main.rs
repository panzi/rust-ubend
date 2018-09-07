extern crate libc;

use std::vec::Vec;
use std::result;
use std::os::unix::io::{FromRawFd, AsRawFd};
use std::collections::HashMap;
use std::ffi::{CStr};
use std::fs::File;
use std::ptr;
use std::mem;

use libc::{
	fork,
	pipe,
	strerror_r, // GNU
	perror,
	open,
	close,
	dup,
	dup2,
	pid_t,
	execvp,
	execvpe, // GNU
	mkstemp,
	unlink,
	exit,
	kill,
	wait,
	c_int,
	c_char,
	__errno_location,
	O_RDONLY,
	O_WRONLY,
	O_RDWR,
	O_TMPFILE, // Linux
	S_IRUSR,
	S_IWUSR,
	EOPNOTSUPP,
	EISDIR,
	ENOENT,
	EINVAL,
	EXIT_FAILURE,
	STDIN_FILENO,
	STDOUT_FILENO,
	STDERR_FILENO,
	SIGTERM
};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Target {
	Stdout,
	Stderr
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PipeSetup {
	Keep,
	Pipe,
	Null,
	Redirect(Target),
	Temp,
	File(c_int)
}

pub struct Pipes {
	stdin:  PipeSetup,
	stdout: PipeSetup,
	stderr: PipeSetup,
	argv: Vec<String>,
	envp: Option<HashMap<String, String>>
}

pub struct Child {
	pid: pid_t,
	pub stdin:  Option<File>,
	pub stdout: Option<File>,
	pub stderr: Option<File>
}

pub struct Chain {
	children: Vec<Child>
}

#[derive(Debug)]
pub struct Error {
	pub errno: c_int
}

pub type Result<T> = result::Result<T, Error>;

macro_rules! c_err {
	() => {
		Err(Error { errno: unsafe { *__errno_location() } })
	}
}

impl Child {
	fn kill(&mut self, sig: c_int) -> Result<()> {
		if unsafe { kill(self.pid, sig) } == -1 {
			return c_err!();
		}
		Ok(())
	}

	fn wait(&mut self) -> Result<c_int> {
		let mut status: c_int = -1;
		if unsafe { wait( &mut status ) } == -1 {
			return c_err!();
		}
		Ok(status)
	}
}

impl Drop for Child {
	fn drop(&mut self) {
		if self.pid >= 0 {
			match self.kill(SIGTERM) { _ => {} }
		}
	}
}

impl Error {
	pub fn to_str(&self) -> String {
		let mut buf = [0u8; 4096];
		let res: c_int = unsafe {
			strerror_r(self.errno, buf.as_mut_ptr() as *mut c_char, buf.len())
		};
		if res == 0 {
			match CStr::from_bytes_with_nul(&buf[..]) {
				Ok(errstr) => {
					return match errstr.to_str() {
						Ok(s) => s.to_string(),
						_ => "error getting error string (broken UTF-8 encoding)".to_string()
					}
				},
				_ => {}
			}
		}

		"error getting error string".to_string()
	}
}

macro_rules! sapwn_unexpected_end {
	($tt:tt) => {}
}

macro_rules! sapwn_unexpected_token {
	() => {}
}

macro_rules! spawn_internal_envp {
	() => {
		None
	};

	($((($($key:tt)*) ($($val:tt)*)))*) => {
		{
			let mut envp = HashMap::new();
			$(envp.insert($($key)*.to_string(), $($val)*.to_string());)*
			Some(envp)
		}
	}
}

macro_rules! spawn_internal_pass {
	($($a:tt)*) => (
		$($a)*
	);
}

macro_rules! spawn_internal_concat {
	($($a:tt)*) => (
		$($a),*
	);
}

macro_rules! spawn_internal {
	(stdin ($($stream:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($cont:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($($cont)* (($($stream)*) ($($stdout)*) ($($stderr)*) ($($argv)*) ($($envp)*)) ($($chain)*))
	};

	(stdout ($($stream:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($cont:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($($cont)* (($($stdin)*) ($($stream)*) ($($stderr)*) ($($argv)*) ($($envp)*)) ($($chain)*))
	};

	(stdin ($($stream:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($cont:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($($cont)* (($($stdin)*) ($($stdout)*) ($($stream)*) ($($argv)*) ($($envp)*)) ($($chain)*))
	};

	(argv ($($exprs:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($cont:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($($cont)* (($($stdin)*) ($($stdout)*) ($($stderr)*) ($($exprst)*) ($($envp)*)) ($($chain)*))
	};

	(envp ($($exprs:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($cont:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($($cont)* (($($stdin)*) ($($stdout)*) ($($stderr)*) ($($argv)*) ($($exprs)*)) ($($chain)*))
	};

	(arg ($($exprs:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($cont:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($($cont)* (($($stdin)*) ($($stdout)*) ($($stderr)*) ($($argv)* ($($exprs)*)) ($($envp)*)) ($($chain)*))
	};

	// TODO: inherit environment and only overload variables?
	(var ($($key:tt)*) ($($val:tt)*) (($($stdin:tt)*) ($($stdout:tt)*) ($($stderr:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($cont:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($($cont)* (($($stdin)*) ($($stdout)*) ($($stderr)*) ($($argv)*) ($($envp)* (($($key)*) ($($val)*)))) ($($chain)*))
	};

	// ========== ENVIRONMENT VARIABLES ========================================
	(@env ($id:ident) ($val:expr) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(var (stringify!($id)) ($val) ($($opts)*) (@head () ($($rest)*)) ($($chain)*))
	};

	(@env ($id:ident) ($($tt:tt)*) ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@env ($id) ($($tt)* $tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	// ========== HEAD =========================================================
	(@head ($id:ident) (= $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@env ($id) () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@head ($id:ident) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(arg (stringify!($id)) ($($opts)*) (@body () ($($rest)*)) ($($chain)*))
	};

	(@head ($id:expr) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(arg ($id) ($($opts)*) (@body () ($($rest)*)) ($($chain)*))
	};

	(@head () ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@head ($tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@head ($tt:tt) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		sapwn_unexpected_token!($tt)
	};

	// ========== BODY =========================================================
	(@end ($($chain:tt)*)) => {
		vec![$($chain),*]
	};

	(@body () () (($($stdout:tt)*) ($($stderr:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($chain:tt)*)) => {
		spawn_internal!(@chain (Pipes {
			stdin: $($stdin)*,
			stdout: $($stdout)*,
			stderr: $($stderr)*,
			argv: vec![$($argv.to_string()),*],
			envp: spawn_internal_envp!($($envp)*)
		}) ($($chain)*) (@end))
	};

	(@body (|) () ($($opts:tt)*) ($($chain:tt)*)) => {
		sapwn_unexpected_end!()
	};

	(@chain ($($elem:tt)*) ($($chain:tt)*) ($($cont:tt)*)) => {
		spawn_internal!($($cont)* ($($chain)* ($($elem)*)))
	};

	(@body (|) ($($rest:tt)*) (($($stdout:tt)*) ($($stderr:tt)*) ($($argv:tt)*) ($($envp:tt)*)) ($($chain:tt)*)) => {
		spawn_internal!(@chain
			(Pipes {
				stdin: $($stdin)*,
				stdout: $($stdout)*,
				stderr: $($stderr)*,
				argv: vec![$($argv.to_string()),*],
				envp: spawn_internal_envp!($($envp)*)
			})
			($($chain)*)
			(@head () ($($rest)*) ((PipeSetup::Pipe) (PipeSetup::Pipe) (PipeSetup::Keep) () ()))
		)
	};

	(@body (<) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe stdin () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body (>) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe stdout () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body (1) (> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe stdin () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body (2) (> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe stdout () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body (3) (> $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe stderr () ($($rest)*) ($($opts)*) ($($chain)*))
	};

	(@body ($arg:expr) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(arg ($arg) ($($opts)*) (@body () ($($rest)*)) ($($chain)*))
	};

	(@body ($($tt:tt)*) ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@body ($($tt)* $tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};

	// ========== PIPE =========================================================
	/* TODO
	(@pipe $pipe:ident (&) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe $pipe ???)
	};
	*/

	(@pipe $pipe:ident (/dev/null) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($pipe (PipeSetup::Null) ($($opts)*) (@body () ($($rest)*)) ($($chain)*))
	};

	// TODO: needs to be much more complex in order to redirect std streams
	(@pipe $pipe:ident ($io:expr) ($($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!($pipe ($io) ($($opts)*) (@body () ($($rest)*)) ($($chain)*))
	};

	(@pipe $pipe:ident ($($tt:tt)*) ($tok:tt $($rest:tt)*) ($($opts:tt)*) ($($chain:tt)*)) => {
		spawn_internal!(@pipe $pipe ($($tt)* $tok) ($($rest)*) ($($opts)*) ($($chain)*))
	};
}

macro_rules! spawn {
	($($tt:tt)*) => {
		// arguments: @marker (current token) (tokens to parse) ((stdin) (stdout) (stderr) (argv) (envp)) (pipe chain array)
		{
			let chain =
				spawn_internal!(@head () ($($tt)*) ((PipeSetup::Keep) (PipeSetup::Pipe) (PipeSetup::Keep) () ()) ());
			Chain::open(&chain[..])
		}
	}
}

struct Fd {
	fd: c_int
}

impl Fd {
	pub fn new() -> Self {
		Fd { fd: -1 }
	}

	pub fn close(&mut self) {
		if self.fd >= 0 {
			unsafe { close(self.fd); }
			self.fd = -1;
		}
	}
}

impl Drop for Fd {
	fn drop(&mut self) {
		self.close();
	}
}

fn open_temp_fd_fallback() -> c_int {
	let mut name: [u8; 17] = *b"/tmp/pipesXXXXXX\0";
	let fd = unsafe { mkstemp(name.as_mut_ptr() as *mut c_char) };
	if fd < 0 {
		return -1;
	}

	if unsafe { unlink(name.as_ptr() as *const c_char) } != 0 {
		unsafe { close(fd); }
		return -1;
	}
	return -1;
}

fn open_temp_fd() -> c_int {
	if cfg!(target_os = "linux") {
		let fd = unsafe { open(b"/tmp\0".as_ptr() as *const c_char, O_TMPFILE | O_RDWR, S_IRUSR | S_IWUSR) };
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

impl Pipes {
	pub fn new(prog: &str) -> Self {
		Pipes {
			stdin:  PipeSetup::Keep,
			stdout: PipeSetup::Keep,
			stderr: PipeSetup::Keep,
			argv: vec![prog.to_string()],
			envp: None
		}
	}

	pub fn first(args: &[&str]) -> Self {
		Pipes {
			stdin:  PipeSetup::Keep,
			stdout: PipeSetup::Pipe,
			stderr: PipeSetup::Keep,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: None
		}
	}

	pub fn last(args: &[&str]) -> Self {
		Pipes {
			stdin:  PipeSetup::Pipe,
			stdout: PipeSetup::Keep,
			stderr: PipeSetup::Keep,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: None
		}
	}

	pub fn pass_through(args: &[&str]) -> Self {
		Pipes {
			stdin:  PipeSetup::Pipe,
			stdout: PipeSetup::Pipe,
			stderr: PipeSetup::Keep,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: None
		}
	}

	pub fn pass_stdin(fd: c_int, args: &[&str]) -> Self {
		Pipes {
			stdin:  PipeSetup::File(fd),
			stdout: PipeSetup::Pipe,
			stderr: PipeSetup::Keep,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: None
		}
	}

	pub fn pass_stdout(fd: c_int, args: &[&str]) -> Self {
		Pipes {
			stdin:  PipeSetup::Pipe,
			stdout: PipeSetup::File(fd),
			stderr: PipeSetup::Keep,
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: None
		}
	}

	pub fn pass_stderr(fd: c_int, args: &[&str]) -> Self {
		Pipes {
			stdin:  PipeSetup::Pipe,
			stdout: PipeSetup::Keep,
			stderr: PipeSetup::File(fd),
			argv: args.iter().map(|arg| arg.to_string()).collect(),
			envp: None
		}
	}

	pub fn open(&self) -> Result<Child> {
		if self.argv.len() == 0 {
			return Err(Error { errno: EINVAL });
		}

		let mut infd  = Fd::new();
		let mut outfd = Fd::new();
		let mut errfd = Fd::new();

		let stdin = match &self.stdin {
			PipeSetup::Keep => None,
			PipeSetup::Pipe => {
				let mut pair: [c_int; 2] = [-1, -1];
				if unsafe { pipe(pair.as_mut_ptr()) } == -1 {
					return c_err!();
				}
				infd.fd = pair[0];
				Some(unsafe { File::from_raw_fd(pair[1]) })
			},
			PipeSetup::Temp => {
				infd.fd = open_temp_fd();
				if infd.fd < 0 {
					return c_err!();
				}
				let fd = unsafe { dup(infd.fd) };
				if fd < 0 {
					return c_err!();
				}
				Some(unsafe { File::from_raw_fd(fd) })
			},
			PipeSetup::Null => {
				infd.fd = unsafe { open(b"/dev/null\0".as_ptr() as *const i8, O_RDONLY) };
				if infd.fd == -1 {
					return c_err!();
				}
				None
			},
			PipeSetup::Redirect(_) => {
				return Err(Error { errno: EINVAL });
			},
			PipeSetup::File(fd) => {
				infd.fd = *fd;
				None
			}
		};

		let stdout = match &self.stdout {
			PipeSetup::Keep => None,
			PipeSetup::Pipe => {
				let mut pair: [c_int; 2] = [-1, -1];
				if unsafe { pipe(pair.as_mut_ptr()) } == -1 {
					return c_err!();
				}
				outfd.fd = pair[1];
				Some(unsafe { File::from_raw_fd(pair[0]) })
			},
			PipeSetup::Temp => {
				outfd.fd = open_temp_fd();
				if outfd.fd < 0 {
					return c_err!();
				}
				let fd = unsafe { dup(outfd.fd) };
				if fd < 0 {
					return c_err!();
				}
				Some(unsafe { File::from_raw_fd(fd) })
			},
			PipeSetup::Null => {
				outfd.fd = unsafe { open(b"/dev/null\0".as_ptr() as *const i8, O_WRONLY) };
				if outfd.fd == -1 {
					return c_err!();
				}
				None
			},
			PipeSetup::Redirect(_) => None,
			PipeSetup::File(fd) => {
				outfd.fd = *fd;
				None
			}
		};

		let stderr = match &self.stderr {
			PipeSetup::Keep => None,
			PipeSetup::Pipe => {
				let mut pair: [c_int; 2] = [-1, -1];
				if unsafe { pipe(pair.as_mut_ptr()) } == -1 {
					return c_err!();
				}
				errfd.fd = pair[1];
				Some(unsafe { File::from_raw_fd(pair[0]) })
			},
			PipeSetup::Temp => {
				errfd.fd = open_temp_fd();
				if errfd.fd < 0 {
					return c_err!();
				}
				let fd = unsafe { dup(errfd.fd) };
				if fd < 0 {
					return c_err!();
				}
				Some(unsafe { File::from_raw_fd(fd) })
			},
			PipeSetup::Null => {
				errfd.fd = unsafe { open(b"/dev/null\0".as_ptr() as *const i8, O_WRONLY) };
				if errfd.fd == -1 {
					return c_err!();
				}
				None
			},
			PipeSetup::Redirect(_) => None,
			PipeSetup::File(fd) => {
				outfd.fd = *fd;
				None
			}
		};

		let pid = unsafe { fork() };

		if pid == -1 {
			return c_err!();
		}

		if pid == 0 {
			// child
			let argv: Vec<Vec<u8>> = self.argv.iter().map(|arg| {
				let mut bytes = arg.clone().into_bytes();
				bytes.push(0);
				bytes
			}).collect();
			let mut c_argv: Vec<*const c_char> = argv.iter().map(|arg| arg[..].as_ptr() as *const c_char).collect();
			c_argv.push(ptr::null());

			redirect_fd(infd.fd,  STDIN_FILENO,  b"redirecting stdin\0".as_ptr() as *const c_char);

			if self.stdout == PipeSetup::Redirect(Target::Stderr) {
				if unsafe { dup2(STDOUT_FILENO, STDERR_FILENO) } == -1 {
					unsafe {
						perror(b"redirecting stdout\0".as_ptr() as *const c_char);
						exit(EXIT_FAILURE);
					}
				}
			} else {
				redirect_fd(outfd.fd, STDOUT_FILENO, b"redirecting stdout\0".as_ptr() as *const c_char);
			}

			if self.stderr == PipeSetup::Redirect(Target::Stdout) {
				if unsafe { dup2(STDERR_FILENO, STDOUT_FILENO) } == -1 {
					unsafe {
						perror(b"redirecting stdout\0".as_ptr() as *const c_char);
						exit(EXIT_FAILURE);
					}
				}
			} else {
				redirect_fd(errfd.fd, STDERR_FILENO, b"redirecting stderr\0".as_ptr() as *const c_char);
			}

			let status = match self.envp {
				Some(ref envp) => {
					let envp: Vec<Vec<u8>> = envp.iter().map(|(key, val)|{
						let mut var = String::new();
						var.push_str(key.as_str());
						var.push_str("=");
						var.push_str(val.as_str());

						let mut bytes = var.into_bytes();
						bytes.push(0);
						bytes
					}).collect();
					let mut c_envp: Vec<*const c_char> = (&envp).iter().map(|var| var[..].as_ptr() as *const c_char).collect();
					c_envp.push(ptr::null());

					unsafe { execvpe(c_argv[0], c_argv.as_ptr(), c_envp.as_ptr()) }
				},
				None => {
					unsafe { execvp(c_argv[0], (&c_argv[..]).as_ptr()) }
				}
			};

			if status == -1 {
				unsafe { perror(c_argv[0]); }
			}
			unsafe { exit(EXIT_FAILURE); }
		} else {
			// parent
			Ok(Child { pid: pid, stdin: stdin, stdout: stdout, stderr: stderr })
		}
	}

	pub fn env(&mut self, key: &str, value: &str) {
		match self.envp {
			Some(ref mut envp) => {
				envp.insert(key.to_string(), value.to_string());
			},
			
			None => {
				let mut envp = HashMap::new();
				envp.insert(key.to_string(), value.to_string());
				let mut envp = Some(envp);
				mem::swap(&mut self.envp, &mut envp);
			}
		}
	}

	pub fn arg(&mut self, arg: &str) {
		self.argv.push(arg.to_string());
	}

	pub fn stdin(&mut self, setup: PipeSetup) {
		match setup {
			PipeSetup::Redirect(_) => {
				self.stdin = PipeSetup::Keep;
			},
			_ => {
				self.stdin = setup;
			}
		}
	}

	pub fn stdout(&mut self, setup: PipeSetup) {
		self.stdout = setup;
	}

	pub fn stderr(&mut self, setup: PipeSetup) {
		self.stderr = setup;
	}
}

impl Chain {
	pub fn open(pipes: &[Pipes]) -> Result<Self> {
		if pipes.len() == 0 {
			return Err(Error { errno: EINVAL });
		}
		let mut children = vec![ pipes[0].open()? ];

		let mut i = 1;
		while i < pipes.len() {
			let pipe = &pipes[i];

			if pipe.stdin == PipeSetup::Pipe {
				let stdin = match &children[i - 1].stdout {
					Some(ref stdout) => stdout.as_raw_fd(),
					None => return Err(Error { errno: EINVAL })
				};
				children.push( Pipes {
					stdin:  PipeSetup::File(stdin),
					stdout: pipe.stdout.clone(),
					stderr: pipe.stderr.clone(),
					argv: pipe.argv.clone(),
					envp: pipe.envp.clone()
				}.open()? );
			} else {
				children.push( pipe.open()? );
			}

			i += 1;
		}

		Ok(Chain { children: children })
	}

	pub fn kill(&mut self, sig: c_int) {
		for ref mut child in &mut self.children {
			match child.kill(sig) { _ => {} };
		}
	}

	pub fn stdin(&mut self) -> Option<&mut File> {
		match &mut self.children[0].stdin {
			Some(ref mut stdin) => Some(stdin),
			None => None
		}
	}

	pub fn stdout(&mut self) -> Option<&mut File> {
		let index = self.children.len() - 1;
		match &mut self.children[index].stdout {
			Some(ref mut stdout) => Some(stdout),
			None => None
		}
	}

	pub fn stderr(&mut self) -> Option<&mut File> {
		let index = self.children.len() - 1;
		match &mut self.children[index].stderr {
			Some(ref mut stderr) => Some(stderr),
			None => None
		}
	}

	pub fn wait(&mut self) -> Result<c_int> {
		let index = self.children.len() - 1;
		self.children[index].wait()
	}
}

fn main() {
	let var = "foo bar";
	let word = "blubb";

	let status = spawn!(
		VAR1="egg spam" VAR2={var} echo "arg1" "arg2" {word} |
		sed {"s/blubb/baz/"} |
		cat
	).expect("spawn failed").wait().expect("wait failed");
	println!("status: {}", status);

	let file = File::open("./src/main.rs").expect("couldn't open main.rs");
	let chain = vec![
		Pipes::pass_stdin(file.as_raw_fd(), &["grep", "new"]),
		Pipes::pass_through(&["cat"]),
		Pipes::last(&["wc", "-l"])
	];
	let status = Chain::open(&chain[..]).expect("chain failed").wait().expect("wait failed");
	println!("status: {}", status);

/*
	spawn!(
		VAR1="egg spam" VAR2={var} getenv "VAR1"
	).expect("spawn failed");
*/
}
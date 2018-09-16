#[macro_use]
extern crate ubend;

use ubend::IntoPipeSetup;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::{Write, Read, Seek, SeekFrom};
use std::path::Path;
use std::fs::remove_file;

macro_rules! assert_output {
	($chain:expr, $stdout:expr, $stderr:expr) => {
		let out = $chain.expect("spawn failed").
		output().expect("reading output failed");

		assert_eq!(
			String::from_utf8(out.stdout).
			expect("reading UTF-8 from stdout failed"),
			$stdout);

		assert_eq!(
			String::from_utf8(out.stderr).
			expect("reading UTF-8 from stderr failed"),
			$stderr);

		assert_eq!(out.status, 0);
	};

	($chain:expr, $stdout:expr) => {
		let out = $chain.expect("spawn failed").
		output().expect("reading output failed");

		assert_eq!(
			String::from_utf8(out.stdout).
			expect("reading UTF-8 from stdout failed"),
			$stdout);

		assert_eq!(out.status, 0);
	};
}

macro_rules! assert_wait {
	($chain:expr) => {
		let status = match $chain.wait_last() {
			Ok(status) => status,
			Err(err) => panic!("wait for last child failed: {}", err)
		};
		assert_eq!(status, 0, "checking exit status of last child");
	}
}

macro_rules! assert_file_contens {
	($file:expr, $contens:expr) => {
		let msg = format!("opening file failed: {}", $file);
		let mut file = File::open($file).expect(msg.as_str());

		let msg = format!("reading file failed: {}", $file);
		let mut s = String::new();
		file.read_to_string(&mut s).expect(msg.as_str());
		assert_eq!(s.as_str(), $contens);
	};
}

fn remove_output_file(name: &str) {
	if Path::new(name).exists() {
		let msg = format!("error removing file: {}", name);
		remove_file(name).expect(msg.as_str());
	}
}

#[test]
fn stdio() {
	assert_output!(
		ubend!("./tests/stdio.sh" 2>ubend::PipeSetup::Pipe),
		"stdout\n", "stderr\n");
}

#[test]
fn read_and_write_file_by_name() {
	remove_output_file("./tests/read_and_write_file_by_name.txt");

	assert_wait!(ubend!(
		grep "spam" <"./tests/input.txt" |
		cat >&1 |
		wc "-l" >"./tests/read_and_write_file_by_name.txt"
	).expect("spawn failed"));

	assert_file_contens!(
		"./tests/read_and_write_file_by_name.txt",
		"3\n");
}

#[test]
fn write_file_by_handle() {
	remove_output_file("./tests/write_file_by_handle.txt");

	let file = OpenOptions::new().
	write(true).
	create(true).
	truncate(true).
	open("./tests/write_file_by_handle.txt").
	expect("couldn't open write_file_by_handle.txt");

	assert_wait!(ubend!(
		grep "spam" <"./tests/input.txt" |
		cat |
		wc "-l" >file
	).expect("spawn failed"));

	assert_file_contens!(
		"./tests/write_file_by_handle.txt",
		"3\n");
}

#[test]
fn read_file_by_handle() {
	let file = File::open("./tests/input.txt").expect("couldn't open input.txt");
	assert_output!(ubend!(
		grep "spam" <file |
		wc "-l"
	), "3\n");
}

#[test]
fn append() {
	remove_output_file("./tests/append.txt");

	assert_wait!(
		ubend!(echo "first line" > "./tests/append.txt").
		expect("spawn failed"));

	assert_file_contens!("./tests/append.txt", "first line\n");

	assert_wait!(
		ubend!(echo "second line" >> "./tests/append.txt").
		expect("spawn failed"));

	assert_file_contens!("./tests/append.txt", "first line\nsecond line\n");
}

#[test]
fn setenv() {
	assert_output!(
		ubend!(FOO="BAR" "./tests/getenv.sh" "FOO"),
		"BAR\n");
}

#[test]
fn temp_file() {
	let mut chain = ubend!(echo "hello world" >ubend::PipeSetup::Temp).
		expect("spawn failed");
	let status = chain.wait_last().expect("wait failed");
	assert_eq!(status, 0);

	let mut temp = chain.stdout().unwrap();
	temp.seek(SeekFrom::Start(0)).expect("seek failed");

	let mut buf = Vec::new();
	temp.read_to_end(&mut buf).expect("read failed");

	assert_eq!(
			String::from_utf8(buf).
			expect("reading UTF-8 failed"),
			"hello world\n");
}

#[test]
fn write_to_null() {
	let state = ubend!(echo "hello world" >ubend::PipeSetup::Null).
	expect("spawn failed").wait_last().expect("wait failed");

	assert_eq!(state, 0);
}

#[test]
fn stdin_as_pipe() {
	let mut chain = ubend!(cat <ubend::PipeSetup::Pipe|cat).expect("spawn failed");
	{
		let mut stream = chain.stdin().unwrap();
		write!(stream, "hello world");
	}

	let out = chain.output().expect("reading output failed");

	assert_eq!(
		String::from_utf8(out.stdout).
		expect("reading UTF-8 from stdout failed"),
		"hello world");

	assert_eq!(out.status, 0);
}

#[test]
fn deep_cat() {
	assert_output!(
		ubend!(echo "hello world"|cat|cat >&1|cat|cat),
		"hello world\n");
}

#[test]
fn strange_setup() {
	use ubend::PipeSetup::{*};

	let mut chain = ubend!(
		echo "hello" "world" <Inherit >Inherit |
		grep "spam" <Pipe |
		wc "-l" <Pipe).expect("spawn failed");

	{
		let mut stream = chain.stdin_at(1).unwrap();
		write!(stream, "spam\neggs\nspam spam\n");
	}

	let output = chain.output().expect("reading output failed");
	assert_eq!(output.status, 0);

	let num = String::from_utf8(output.stdout).
		expect("reading UTF-8 failed");
	assert_eq!(num.trim(), "2");
}

#[test]
fn command_as_var() {
	let cmd = "echo";
	assert_output!(ubend!({cmd} "hello world"), "hello world\n");
}

#[test]
fn spread_args() {
	let args = ["foo", "bar", "baz"];
	assert_output!(ubend!(echo "egg" args... "bacon"), "egg foo bar baz bacon\n");

	let vec_args = vec!["foo", "bar", "baz"];
	assert_output!(ubend!(echo "egg" vec_args... "bacon"), "egg foo bar baz bacon\n");

	assert_output!(ubend!(echo "egg" ["foo", "bar", "baz"]... "bacon"), "egg foo bar baz bacon\n");
}
#[macro_use]
extern crate ubend;

use ubend::IntoPipeSetup;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::{Read, Seek, SeekFrom};
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
		cat >&1 |
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
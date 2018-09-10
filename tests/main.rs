#[macro_use]
extern crate pipes;

use pipes::IntoPipeSetup;
use std::fs::File;
use std::fs::OpenOptions;
use std::io::Read;
use std::path::Path;
use std::fs::remove_file;

macro_rules! assert_output {
	($chain:expr, $stdout:expr, $stderr:expr) => {
		let out = $chain.expect("spawn failed").
		output().expect("reading output failed");

		assert_eq!(out.status, 0);

		assert_eq!(
			String::from_utf8(out.stdout).
			expect("reading UTF-8 from stdout failed"),
			$stdout);

		assert_eq!(
			String::from_utf8(out.stderr).
			expect("reading UTF-8 from stderr failed"),
			$stderr);
	};

	($chain:expr, $stdout:expr) => {
		let out = $chain.expect("spawn failed").
		output().expect("reading output failed");

		assert_eq!(out.status, 0);
		assert_eq!(
			String::from_utf8(out.stdout).
			expect("reading UTF-8 from stdout failed"),
			$stdout);
	};
}

macro_rules! assert_wait {
	($chain:expr) => {
		for (index, status) in ($chain).wait().iter().enumerate() {
			let status = match status {
				Ok(status) => *status,
				Err(err) => panic!("wait for child {} failed: {}", index, err)
			};
			assert_eq!(status, 0, "checking exit status of child {}", index);
		}
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

// FIXME: there are weird random crashes where I have no idea what's happening
#[test]
fn stdio() {
	assert_output!(
		spawn!("./tests/stdio.sh" 2>pipes::PipeSetup::Pipe),
		"stdout\n", "stderr\n");
}

#[test]
fn read_and_write_file_by_name() {
	remove_output_file("./tests/read_and_write_file_by_name.txt");

	assert_wait!(spawn!(
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

	assert_wait!(spawn!(
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
	assert_output!(spawn!(
		grep "spam" <file |
		cat |
		wc "-l"
	), "3\n");
}

#[test]
fn append() {
	remove_output_file("./tests/append.txt");

	assert_wait!(
		spawn!(echo "first line" > "./tests/append.txt").
		expect("spawn failed"));

	assert_file_contens!("./tests/append.txt", "first line\n");

	assert_wait!(
		spawn!(echo "second line" >> "./tests/append.txt").
		expect("spawn failed"));

	assert_file_contens!("./tests/append.txt", "first line\nsecond line\n");
}

#[test]
fn setenv() {
	assert_output!(
		spawn!(FOO="BAR" "./tests/getenv.sh" "FOO"),
		"BAR\n");

}

// TODO: write tests, fix bugs
/*
fn main() {
	stdio();
	read_and_write_file_by_name();
	write_file_by_handle();
	read_file_by_handle();
	append();
	setenv();
}
*/
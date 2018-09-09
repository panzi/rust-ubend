#[macro_use]
extern crate pipes;

use pipes::ToPipeSetup;
use std::fs::File;

// TODO: find out how to actually write tests
fn main() {
//	let var = "foo bar";
//	let word = "blubb";
/*
	let statuses = spawn!(
		VAR1="egg spam" VAR2={var} echo "arg1" "arg2" {word} |
		sed {"s/blubb/baz/"} |
		cat >PipeSetup::Inherit
	).expect("spawn failed").wait().expect("wait failed");
	println!("status: {}", statuses);
*/
/*
	let statuses = spawn!(
		echo "hello world" >PipeSetup::Inherit
	).expect("spawn failed").wait();
	println!("statuses: {:?}", statuses);

	let statuses = spawn!(
		echo "hello world 2" >PipeSetup::Inherit
	).expect("spawn failed").wait();
	println!("statuses: {:?}", statuses);

	let statuses = spawn!(
		echo "hello world and cat" >&1 | cat >PipeSetup::Inherit
	).expect("spawn failed").wait();
	println!("statuses: {:?}", statuses);
*/
/*
	let file = File::open("./src/main.rs").expect("couldn't open main.rs");
	let mut chain = vec![
		Pipes::pass_stdin(file.as_raw_fd(), &["grep", "new"]),
		Pipes::pass_through(&["cat"]),
		Pipes::last(&["wc", "-l"])
	];
//	chain[0].stdin(PipeSetup::Inherit);
	let status = Chain::open(&chain[..]).expect("chain failed").wait().expect("wait failed");
	println!("status: {}", status);
*/
/*
	spawn!(
		VAR1="egg spam" VAR2={var} getenv "VAR1"
	).expect("spawn failed");
*/
	let statuses = spawn!(
		grep "new" <"./src/main.rs" |
		cat >&1 |
		wc "-l" >>"./tests/out.txt"
	).expect("spawn failed").wait();
	println!("statuses: {:?}", statuses);
	for status in statuses {
		assert!(status == Ok(0));
	}

	let file = File::open("./src/main.rs").expect("couldn't open main.rs");
	let statuses = spawn!(
		grep "new" <file |
		cat |
		wc "-l" >pipes::PipeSetup::Inherit
	).expect("spawn failed").wait();
	println!("statuses: {:?}", statuses);
	for status in statuses {
		assert!(status == Ok(0));
	}

	let statuses = spawn!(
		FOO="BAR" "./tests/getenv.sh" "FOO" >pipes::PipeSetup::Inherit
	).expect("spawn failed").wait();
	println!("statuses: {:?}", statuses);
	for status in statuses {
		assert!(status == Ok(0));
	}

	let statuses = spawn!(
		"./tests/stdio.sh" 2>"./tests/stderr.txt"
	).expect("spawn failed").wait();
	println!("statuses: {:?}", statuses);
	for status in statuses {
		assert!(status == Ok(0));
	}
}
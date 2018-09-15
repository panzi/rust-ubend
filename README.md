U-Bend
======


This is a small crate that lets you build pipe chains between spawned
processes using a syntax similar to the Unix shell.

```rust
# #[macro_use] extern crate ubend;
# use ubend::IntoPipeSetup;

let output = spawn!(
		cat <"./tests/input.txt" |
		grep "spam" |
		wc "-l"
	).expect("spawn failed").
	output().
	expect("reading output failed").
	stdout;

let output = String::from_utf8(output).
	expect("UTF-8 error").
	trim().
	parse::<i64>().
	unwrap();

println!("output: {}", output);
```

Note that arguments to commands need always to be quoted. If you don't quote
them they are interpreted as Rust identifiers which allows you to pass
dynamic strings. Also `FOO="bar" cat <"baz"` is the same as
`FOO = "bar" cat < "baz"`, since whitespace is ignored in Rust.

`use ubend::IntoPipeSetup` is needed when passing a file name or
`std::fs::File` as redirection source/target.

**Note:** Currently only Linux ist tested. Other Unix operating systems might
work, too. Windows support is not implemented.

## More Examples

```rust
#[macro_use] extern crate ubend;
use ubend::IntoPipeSetup;
use ubend::PipeSetup::*;

use std::fs::File;
use std::io::{Read, Seek, SeekFrom};

// Ignore stderr
spawn!(rm "no_such_file" 2>Null);

// Redirect stderr to stdout
spawn!(rm "no_such_file" 2>&1);

// Write stderr to stderr of this process
spawn!(rm "no_such_file" 2>Inherit);

// Read from a file opened in Rust
let file = File::open("./tests/input.txt").
	expect("couldn't open file");
spawn!(grep "spam" <file);

// Write stderr to a temp file
let mut chain = spawn!(rm "no_such_file" 2>Temp).
	expect("spawn failed");

chain.wait_last().
	expect("wait failed");

let mut temp = chain.stderr().unwrap();

// Since the file descriptor was shared with the spawned process the
// position needs to be reset manually:
temp.seek(SeekFrom::Start(0)).
	expect("seek failed");
```

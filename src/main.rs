use std::borrow::{Borrow, BorrowMut, Cow};
use std::fmt::Debug;
use std::io;
use std::io::{BufRead, BufReader, BufWriter, Read, Write};
use std::path::PathBuf;
use std::error::Error;

use gumdrop::Options;

mod token_data;
mod line_numbers;
mod latin1;

#[derive(Debug, Options)]
enum Command {

	#[options(help = "convert a text file to BASIC")]
	Pack(PackArgs),

	#[options(help = "convert a BASIC file to text")]
	Unpack(UnpackArgs),
}

#[derive(Debug, Options)]
struct PackArgs {

	#[options(help = "ASCII input file to encode")]
	input_file: PathBuf,

	#[options(help = "output BASIC file")]
	output_file: PathBuf,

	#[options(short = "b", long = "bbc", help = "use BBC Micro version file")]
	use_bbc_format: bool,

	#[options(help = "show help for this command")]
	help: bool,
}

#[derive(Debug, Options)]
struct UnpackArgs {

	#[options(help = "BASIC file to decode")]
	input_file: String,

	#[options(help = "output ASCII file")]
	output_file: String,

	#[options(help = "`true` to always output line numbers, `false` to reject code that requires them")]
	force_line_numbers: Option<bool>,

	#[options(help = "show help for this command")]
	help: bool,
}

impl Command {
	fn user_wants_help(&self) -> bool {
		match *self {
			Command::Pack(ref c) if c.help => true,
			Command::Unpack(ref c) if c.help => true,
			_ => false,
		}
	}

	fn print_usage_and_exit(command_name: Option<&'static str>) -> ! {
		let to_print = command_name.and_then(Command::command_usage)
			.unwrap_or_else(Command::usage);
		eprintln!("{}", to_print);
		std::process::exit(ExitCode::Success.into());
	}
}

#[repr(i32)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ExitCode {
	Success = 0,
	InternalError = 1,
	IoError = 2,
	CliArgError = 3,
	InvalidData = 4,
}

impl From<ExitCode> for i32 {
	fn from(src: ExitCode) -> Self {
		src as i32
	}
}

struct ByteIo<R: BufRead>(pub R);

impl<R: BufRead> Debug for ByteIo<R> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str("ByteIo")
	}
}

impl<R: BufRead> Iterator for ByteIo<R> {
	type Item = io::Result<u8>;

	fn next(&mut self) -> Option<Self::Item> {
		match self.0.fill_buf() {
			Ok(&[x, ..]) => Some(Ok(x)),
			Ok(&[]) => None,
			Err(e) => Some(Err(e)),
		}
	}
}

#[derive(Debug)]
enum Input<'a> {
	Stdio(std::io::StdinLock<'a>),
	File(std::fs::File),
}

#[derive(Debug)]
enum Output<'a> {
	Stdio(std::io::StdoutLock<'a>),
	File(std::fs::File),
}

macro_rules! _impl_io_borrows {
	($enum:ident as $trait:path) => {
		impl<'s, 'a: 's> Borrow<dyn $trait + 's> for $enum<'a> {
			fn borrow(&self) -> &(dyn $trait + 's) {
				match *self {
					Self::Stdio(ref stdio) => stdio,
					Self::File(ref file) => file,
				}
			}
		}

		impl<'s, 'a: 's> BorrowMut<dyn $trait + 's> for $enum<'a> {
			fn borrow_mut(&mut self) -> &mut (dyn $trait + 's) {
				match *self {
					Self::Stdio(ref mut stdio) => stdio,
					Self::File(ref mut file) => file,
				}
			}
		}
	};
}

_impl_io_borrows!(Input as Read);
_impl_io_borrows!(Output as Write);


fn main() {
	let (process, args_str) = {
		let mut iter = std::env::args();
		let process = iter.next().map(Cow::Owned).unwrap_or(
			Cow::Borrowed(env!("CARGO_PKG_NAME"))
		);
		(process, iter.collect::<Vec<_>>())
	};
	if matches!(*args_str, [ref s] if s == "help") {
		Command::print_usage_and_exit(None);
	}
	let args = match Command::parse_args_default(&*args_str) {
		Ok(a) if a.user_wants_help() => Command::print_usage_and_exit(a.command_name()),
		Ok(a) => a,
		Err(e) => {
			eprintln!("argument error: {}", e);
			eprintln!("run `{} help` for usage guidelines", process);
			std::process::exit(ExitCode::CliArgError.into());
		}
	};

	let mut unpack_error: Option<token_data::UnpackError> = None;

	let result : Result<(), (&dyn Error, ExitCode)> = match args {
		Command::Unpack(args) => run_unpack(args).map_err(|e| {
			let exit_code = match e {
				UnpackError::UnexpectedEof | UnpackError::IoError(_) => ExitCode::IoError,
				_ => ExitCode::InvalidData,
			};

			let error_reborrowed = unpack_error.insert(e);
			use token_data::UnpackError;

			(error_reborrowed as &dyn Error, exit_code)
		}),
		_ => {
			eprintln!("not supported yet, sorry");
			std::process::exit(ExitCode::InternalError.into());
		}
	};

	std::process::exit(if let Err((e, code)) = result {
		eprintln!("error: {}", e);
		code.into()
	} else { ExitCode::Success.into() });
}

fn run_unpack(args: UnpackArgs) -> Result<(), token_data::UnpackError> {
	let stdin = io::stdin();
	let stdout = io::stdout();

	let mut output = match &*args.output_file {
		"-" => Output::Stdio(stdout.lock()),
		path => Output::File(std::fs::File::create(path)?)
	};
	let output: &mut dyn Write = output.borrow_mut();
	let mut output = BufWriter::new(output);

	let mut input = match &*args.input_file {
		"-" => Input::Stdio(stdin.lock()),
		path => Input::File(std::fs::File::open(path)?),
	};
	let input: &mut dyn Read = input.borrow_mut();
	let input = ByteIo(BufReader::new(input));

	let mut utf8_buffer = [0u8; 4];
	for byte in token_data::TokenUnpacker::new(input) {
		let this_char = byte?.encode_utf8(&mut utf8_buffer).as_bytes();
		output.write_all(this_char)?;
	}

	Ok(())
}

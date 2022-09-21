use std::borrow::Cow;
use std::fmt::Debug;
use std::fs;
use std::io::{self, BufReader, BufWriter, Write as _};
use std::path::PathBuf;
use std::error::Error;

use gumdrop::Options;

mod common;
mod support;
mod token_data;
mod line_numbers;
mod latin1;
mod unpack;

use unpack::UnpackError;

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

	#[options(help = "use of line numbers in output (minimal, always, forbid)",
		default = "minimal",
		parse(try_from_str = "UnpackLineNumbersOption::try_parse"),
		)]
	use_line_numbers: UnpackLineNumbersOption,

	#[options(help = "show help for this command")]
	help: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UnpackLineNumbersOption {
	Minimal,
	AlwaysShow,
	ForbidUse,
}

impl UnpackLineNumbersOption {
	fn try_parse(input: &str) -> Result<Self, &'static str> {
		match input {
			"minimal" | "" => Ok(Self::Minimal),
			"always" => Ok(Self::AlwaysShow),
			"forbid" => Ok(Self::ForbidUse),
			_ => Err("invalid value for force_line_numbers")
		}
	}
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

	let mut unpack_error: Option<UnpackError> = None;

	let result : Result<(), (&dyn Error, ExitCode)> = match args {
		Command::Unpack(args) => run_unpack(args).map_err(|e| {
			let exit_code = match e {
				UnpackError::UnexpectedEof | UnpackError::IoError(_) => ExitCode::IoError,
				_ => ExitCode::InvalidData,
			};

			(unpack_error.insert(e) as &dyn Error, exit_code)
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

fn run_unpack(args: UnpackArgs) -> Result<(), unpack::UnpackError> {
	use crate::latin1::CharExt;

	let stdin;
	let mut stdin_lock;
	let mut stdout_lock;

	let stdout;
	let mut input_file;
	let mut output_file;

	// Set output io objects
	let output: &mut dyn io::Write = match &*args.output_file {
		"-" => {
			stdout = io::stdout();
			stdout_lock = stdout.lock();
			&mut stdout_lock
		},
		path => {
			input_file = fs::File::create(path)?;
			&mut input_file
		}
	};
	let mut output = BufWriter::new(output);

	let input: &mut dyn io::BufRead = match &*args.input_file {
		"-" => {
			stdin = io::stdin();
			stdin_lock = stdin.lock();
			&mut stdin_lock
		},
		path => {
			output_file = BufReader::new(fs::File::open(path)?);
			&mut output_file
		},
	};

	let mut parser = unpack::Parser::new(input);
	let lines = {
		let mut v = Vec::new();
		while let Some(line) = parser.next_line()? {
			v.push(line);
		}
		v
	};
	let there_are_any_referenced_lines = parser.referenced_lines().any();
	if there_are_any_referenced_lines
		&& args.use_line_numbers == UnpackLineNumbersOption::ForbidUse
	{
		// TODO this should not be in UnpackError, really
		return Err(UnpackError::DisallowedLineReference);
	}

	for line in lines {
		match (args.use_line_numbers, parser.referenced_lines().get(line.line_number)) {
			(UnpackLineNumbersOption::AlwaysShow, _) |
			(UnpackLineNumbersOption::Minimal, true)
				=> write!(output, "{:5} ", line.line_number)?,

			(UnpackLineNumbersOption::Minimal, false) if there_are_any_referenced_lines
				=> output.write_all(&[32u8; 6][..])?,

			_ => {},
		};

		let mut utf8_buf = [0u8; 4];
		for latin1_byte in line.data.iter().copied() {
			output.write_all(
				char::from_risc_os_latin1(latin1_byte)
				.encode_utf8(&mut utf8_buf)
				.as_bytes()
				)?;
		}
		writeln!(output)?;
	}

	output.flush()?;

	Ok(())
}

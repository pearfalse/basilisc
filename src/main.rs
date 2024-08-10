use std::borrow::Cow;
use std::fmt::Debug;
use std::fs;
use std::io::{self, BufReader, BufWriter, Write as _};
use std::error::Error;

use gumdrop::Options;

pub mod support;
pub mod token_data;
pub mod line_numbers;
pub mod latin1;
pub mod unpack;
pub mod pack;

#[cfg(doc)]
pub use basilisc_base::keyword;

#[cfg(doc)]
pub use basilisc_base::subarray;

use pack::Error as PackError;
use support::IoObject;

#[derive(Debug, Options)]
enum Command {

	#[options(help = "convert a text file to BASIC")]
	Pack(PackArgs),

	#[options(help = "convert a BASIC file to text")]
	Unpack(UnpackArgs),
}

#[derive(Debug, Options)]
struct PackArgs {

	#[options(help = "ASCII input file to encode", required)]
	input_file: String,

	#[options(help = "output BASIC file", required)]
	output_file: String,

	#[options(help = "show help for this command")]
	help: bool,
}

#[derive(Debug, Options)]
struct UnpackArgs {

	#[options(help = "BASIC file to decode", required)]
	input_file: String,

	#[options(help = "output ASCII file", required)]
	output_file: String,

	#[options(help = "use of line numbers in output (minimal, always, forbid)",
		default = "minimal",
		parse(try_from_str = "UnpackLineNumbersOption::try_parse"),
		)]
	use_line_numbers: UnpackLineNumbersOption,

	#[options(help = "show help for this command")]
	help: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
enum UnpackError {
	#[error("{}", .0)]
	Forwarded(unpack::UnpackError),
	#[error("line reference found; rejecting program")]
	DisallowedLineReference,
}

impl<T> From<T> for UnpackError where unpack::UnpackError: From<T> {
	fn from(src: T) -> Self {
		Self::Forwarded(src.into())
	}
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

	fn print_usage_and_exit(process_name: &str, command_name: Option<&'static str>) -> ! {
		let to_print = command_name.and_then(Command::command_usage)
			.unwrap_or_else(Command::usage);
		eprintln!("{}", to_print);
		eprintln!("\nRun '{} `subcommand` --help' for more guidance", process_name);
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


trait HelpExt: Eq + PartialEq<str> {
	fn is_help(&self) -> bool {
		["help", "-h", "--help", "-help"].into_iter().any(|help| self == help)
	}
}
impl HelpExt for str {}


fn main() {
	let (process, args_str) = {
		let mut iter = std::env::args();
		let process = iter.next().map(Cow::Owned).unwrap_or(
			Cow::Borrowed(env!("CARGO_PKG_NAME"))
		);
		(process, iter.collect::<Vec<_>>())
	};
	if matches!(*args_str, [ref s] if s.is_help()) {
		Command::print_usage_and_exit(&process, None);
	}
	let args = match Command::parse_args_default(&args_str) {
		Ok(a) if a.user_wants_help() => Command::print_usage_and_exit(&process, a.command_name()),
		Ok(a) => a,
		Err(e) => {
			eprintln!("argument error: {}", e);
			eprintln!("run `{} help` for usage guidelines", process);
			std::process::exit(ExitCode::CliArgError.into());
		}
	};

	let mut unpack_error: Option<UnpackError> = None;
	let mut pack_error: Option<PackError> = None;

	let result : Result<(), (&dyn Error, ExitCode)> = match args {
		Command::Unpack(args) => run_unpack(args).map_err(|e| {
			let exit_code = match e {
				UnpackError::Forwarded(unpack::UnpackError::UnexpectedEof)
				| UnpackError::Forwarded(unpack::UnpackError::IoError(_)) => ExitCode::IoError,
				_ => ExitCode::InvalidData,
			};

			(unpack_error.insert(e) as &dyn Error, exit_code)
		}),
		Command::Pack(args) => run_pack(args).map_err(|e| {
			let exit_code = match e {
				PackError::IoError(_) => ExitCode::IoError,
				_ => ExitCode::InvalidData,
			};

			(pack_error.insert(e) as &dyn Error, exit_code)
		}),
		#[allow(unreachable_patterns)]
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

fn run_unpack(args: UnpackArgs) -> Result<(), UnpackError> {
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
			output_file = fs::File::create(path)?;
			&mut output_file
		}
	};
	let mut output = BufWriter::new(output);

	let input: &mut dyn io::Read = match &*args.input_file {
		"-" => {
			stdin = io::stdin();
			stdin_lock = stdin.lock();
			&mut stdin_lock
		},
		path => {
			input_file = BufReader::new(fs::File::open(path)?);
			&mut input_file
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
	let there_are_any_referenced_lines = parser.referenced_lines().iter_set()
		.next().is_some();
	if there_are_any_referenced_lines
		&& args.use_line_numbers == UnpackLineNumbersOption::ForbidUse
	{
		return Err(UnpackError::DisallowedLineReference);
	}

	for line in lines {
		match (args.use_line_numbers, parser.referenced_lines().get(line.line_number)) {
			(UnpackLineNumbersOption::AlwaysShow, _) |
			(UnpackLineNumbersOption::Minimal, true)
				=> write!(output, "{:5}", line.line_number)?,

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

fn run_pack(args: PackArgs) -> Result<(), PackError> {
	let stdin;
	let stdout;

	let mut stdin_lock;
	let mut stdout_lock;

	let mut input_file;
	let mut output_file;

	let output: &mut dyn io::Write = match &*args.output_file {
		"-" => {
			stdout = io::stdout();
			stdout_lock = stdout.lock();
			&mut stdout_lock
		},
		path => {
			output_file = fs::File::options()
				.write(true)
				.truncate(true)
				.open(path)?;
			&mut output_file
		},
	};

	let input: IoObject<'_> = match &*args.input_file {
		"-" => {
			stdin = io::stdin();
			stdin_lock = stdin.lock();
			&mut stdin_lock
		},
		path => {
			input_file = BufReader::new(fs::File::open(path)?);
			&mut input_file
		},
	};

	let mut parser = pack::Parser::new(input);
	while parser.next_line()? { }

	parser.write(output)?;
	Ok(())
}

use gumdrop::Options;

mod token_data;
mod line_numbers;

#[derive(Debug, Options)]
enum Command {
	Encode(EncodeArgs),
	Decode(DecodeArgs),
}

#[derive(Debug, Options)]
struct EncodeArgs {

	#[options(help = "ASCII input file to encode")]
	input_file: String,

	#[options(help = "output BASIC file")]
	output_file: String,

	#[options(short = "b", long = "bbc", help = "use BBC Micro version file")]
	use_bbc_format: bool,
}

#[derive(Debug, Options)]
struct DecodeArgs {

	#[options(help = "BASIC file to decode")]
	input_file: String,

	#[options(help = "output ASCII file")]
	output_file: String,

	#[options(help = "`true` to always output line numbers, `false` to reject code that requires them")]
	force_line_numbers: Option<bool>,
}


fn main() {
    println!("Hello, world!");
}

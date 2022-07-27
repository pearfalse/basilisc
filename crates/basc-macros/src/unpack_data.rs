use ascii::{AsciiString, AsciiStr, AsciiChar};

type UnpackMap<'a> = [Option<&'a AsciiStr>; 256];

#[derive(Debug)]
pub(crate) struct UnpackTable<'a> {
	tokens_direct: UnpackMap<'a>,
	tokens_8d_c6:  UnpackMap<'a>,
	tokens_8d_c7:  UnpackMap<'a>,
	tokens_8d_c8:  UnpackMap<'a>,
}

impl<'a> UnpackTable<'a> {
	fn new(arg: Type) -> RetType {
		unimplemented!()
	}
}

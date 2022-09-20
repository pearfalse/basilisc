use std::borrow::Borrow;

#[derive(Debug)]
pub struct Line<S = Box<[u8]>> {
	pub line_number: u16,
	pub data: S,
}

impl<S: Borrow<[u8]>> Line<S> {
	pub fn new(line_number: u16, data: S) -> Self {
		Self { line_number, data }
	}
}

impl<S: Borrow<[u8]>> Borrow<[u8]> for Line<S> {
	fn borrow(&self) -> &[u8] {
		self.data.borrow()
	}
}

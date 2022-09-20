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

impl<SA: Borrow<[u8]>, SB: Borrow<[u8]>> PartialEq<Line<SB>> for Line<SA> {
	fn eq(&self, other: &Line<SB>) -> bool {
		self.line_number == other.line_number
		&& self.data.borrow() == other.data.borrow()
	}
}

impl<SA: Borrow<[u8]>> Eq for Line<SA> { }

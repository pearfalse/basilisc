use std::{
	borrow::{Borrow, BorrowMut},
	io,
	ops::{Deref, DerefMut},
};

#[derive(Debug)]
pub struct Line<S = Vec<u8>> {
	pub line_number: u16,
	data: S,
}

impl<S: io::Write + Default> Line<S> {
	pub fn new(line_number: u16) -> Self {
		Self {
			line_number,
			data: S::default(),
		}
	}
}

impl<S: io::Write> Line<S> {
	pub fn new_with_writer(line_number: u16, writer: S) -> Self {
		Self {
			line_number,
			data: writer,
		}
	}
}

impl<S: BorrowMut<Vec<u8>>> Borrow<[u8]> for Line<S> {
	fn borrow(&self) -> &[u8] {
		&*self.data.borrow()
	}
}

impl<S: BorrowMut<Vec<u8>>> BorrowMut<[u8]> for Line<S> {
	fn borrow_mut(&mut self) -> &mut [u8] {
		&mut *self.data.borrow_mut()
	}
}

impl<S: Borrow<[u8]>> Deref for Line<S> {
	type Target = [u8];

	fn deref(&self) -> &Self::Target {
		self.data.borrow()
	}
}

impl<S: BorrowMut<[u8]>> DerefMut for Line<S> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		self.data.borrow_mut()
	}
}

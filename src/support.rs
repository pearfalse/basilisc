use std::{io, convert::Infallible};

mod per_line_bits;
pub(crate) use per_line_bits::*;

pub trait NextByte {
	type Error;
	fn next_byte(&mut self) -> Result<Option<u8>, Self::Error>;
}

impl<'a> NextByte for dyn io::Read + 'a {
	type Error = io::Error;

	fn next_byte(&mut self) -> Result<Option<u8>, Self::Error> {
		next_byte_io_read(self)
	}
}

impl<'a, 'b> NextByte for &'a mut (dyn io::Read + 'b) {
	type Error = io::Error;

	fn next_byte(&mut self) -> Result<Option<u8>, Self::Error> {
		next_byte_io_read(self)
	}
}

fn next_byte_io_read<I: io::Read + ?Sized>(src: &mut I) -> Result<Option<u8>, io::Error> {
	let mut tgt = [0u8];
	match io::Read::read(src, &mut tgt) {
		Ok(0) => Ok(None),
		Ok(_) => Ok(Some(tgt[0])),
		Err(e) => Err(e),
	}
}

impl<'a> NextByte for std::slice::Iter<'a, u8> {
	type Error = Infallible;

	fn next_byte(&mut self) -> Result<Option<u8>, Self::Error> {
		Ok(self.next().copied())
	}
}


pub(crate) trait ArrayVecExt {
	fn remove_first(&mut self, x: usize);
}

impl<T, const N: usize> ArrayVecExt for arrayvec::ArrayVec<T, N> {
	fn remove_first(&mut self, x: usize) {
		let old_len = self.len();
		let new_len = old_len.checked_sub(x as usize).unwrap();
		unsafe {
			let data = self.as_mut_ptr();
			std::ptr::copy(data.add(x), data, new_len);
			self.set_len(new_len);
		}
	}
}

#[cfg(test)]
mod test_arrayvec_ext {
    use arrayvec::ArrayVec;

    use super::ArrayVecExt;

	#[test]
	fn remove_first() {
		let mut av: ArrayVec<u8, 9> = ArrayVec::new();
		for i in 20..29 {
			av.push(i);
		}

		av.remove_first(3);

		assert_eq!(&[23, 24, 25, 26, 27, 28], &*av);
	}
}


// include meta-src files that we want
#[path = "../meta-src/keyword.rs"] mod keyword;
#[path = "../meta-src/token_iter.rs"] mod token_iter;
#[path = "../meta-src/subarray.rs"] mod subarray;

pub(crate) use keyword::{
	RawKeyword,
	Keyword,
	IntoIter as KeywordIntoIter,
	MAX_LEN as MAX_KEYWORD_LEN,
};
pub(crate) use token_iter::TokenIter;
pub(crate) use subarray::SubArray;

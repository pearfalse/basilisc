use core::{iter, slice};
use std::fmt;

pub struct PerLineBits {
	store: Box<[u8; Self::BYTE_COUNT]>,
}

impl fmt::Debug for PerLineBits {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "PerLineBits {{ store: {} bytes }}", Self::BYTE_COUNT)
	}
}

impl PerLineBits {
	const BYTE_COUNT: usize = 0xff00 >> 3;

	pub fn new() -> Self {
		Self {
			store: Box::new([0u8; Self::BYTE_COUNT])
		}
	}

	pub fn any(&self) -> bool {
		self.store.iter().any(|&b| b != 0)
	}

	pub fn try_get(&self, index: u16) -> Option<bool> {
		let (byte_idx, bit_mask) = Self::decompose(index);
		self.store.get(byte_idx as usize).map(|r| *r & bit_mask != 0)
	}

	pub fn try_get_mut<'a>(&'a mut self, index: u16) -> Option<BitRefMut<'a>> {
		let (byte_idx, bit_mask) = Self::decompose(index);
		self.store.get_mut(byte_idx as usize).map(|rm| BitRefMut {
			r#ref: rm,
			bit_mask,
		})
	}


	pub fn get(&self, index: u16) -> bool {
		self.try_get(index).expect("index out of range")
	}

	pub fn get_mut<'a>(&'a mut self, index: u16) -> BitRefMut<'a> {
		self.try_get_mut(index).expect("index out of range")
	}

	pub fn iter(&self) -> Iter<'_> {
		Iter::new(self)
	}

	pub fn iter_set(&self) -> IterSet {
		IterSet::new(self)
	}

	pub fn iter_clear(&self) -> IterClear {
		IterClear::new(self)
	}


	fn decompose(index: u16) -> (u16, u8) {
		debug_assert!(Self::BYTE_COUNT < u16::MAX as usize);

		((index >> 3), 1 << (index & 0b111) as u8)
	}
}


#[derive(Debug)]
pub struct BitRefMut<'a> {
	r#ref: &'a mut u8,
	bit_mask: u8,
}

impl<'a> BitRefMut<'a> {
	#[inline]
	pub fn bit(&self) -> bool {
		(*self.r#ref & self.bit_mask) != 0
	}

	#[inline]
	pub fn set(&mut self) {
		*self.r#ref |= self.bit_mask;
	}

	#[inline]
	pub fn clear(&mut self) {
		*self.r#ref &= !self.bit_mask;
	}

	#[inline]
	pub fn set_to(&mut self, value: bool) {
		if value { self.set() } else { self.clear() }
	}
}


#[derive(Debug)]
pub struct Iter<'a> {
	bytes: slice::Iter<'a, u8>,
	cur_byte: Option<u8>,
	bit_mask: u8,
}

impl<'a> Iter<'a> {
	fn new(upper: &'a PerLineBits) -> Self {
		Self {
			bytes: upper.store.iter(),
			cur_byte: None,
			bit_mask: 1,
		}
	}
}

impl<'a> Iterator for Iter<'a> {
	type Item = bool;

	fn next(&mut self) -> Option<Self::Item> {
		let byte = match self.cur_byte {
			Some(b) => b,
			None => *self.cur_byte.insert(*self.bytes.next()?),
		};

		let r = byte & self.bit_mask != 0;
		self.bit_mask = match self.bit_mask.checked_shl(1) {
			Some(new_value) => new_value,
			None => {
				self.cur_byte = None;
				1
			},
		};

		Some(r)
	}
}


pub struct IterFiltered<'a, const S: bool = true> {
	upper: iter::Enumerate<iter::Copied<slice::Iter<'a, u8>>>,
	cur_byte: Option<(usize, u8)>,
	bit_pos: u8,
}

impl<'a, const S: bool> fmt::Debug for IterFiltered<'a, S> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "IterFiltered<{}>", S)?;
		f.debug_struct("")
			.field("cur_byte", &IterFilteredPos(self))
			.field("bit_pos", &format_args!("1<<{}", self.bit_pos))
			.finish()
	}
}

struct IterFilteredPos<'a, const S: bool>(&'a IterFiltered<'a, S>);
impl<'a, const S: bool> fmt::Debug for IterFilteredPos<'a, S> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self.0.cur_byte {
			Some((idx, byte)) => write!(f, "{}/{:02x}", idx, byte),
			None => f.write_str("None"),
		}	
	}
}

impl<'a, const S: bool> IterFiltered<'a, S> {

	fn new(upper: &'a PerLineBits) -> Self {
		Self {
			upper: upper.store.iter().copied().enumerate(),
			cur_byte: None,
			bit_pos: 0,
		}
	}

	const fn skip_clue() -> u8 {
		if S { 0x00 } else { 0xff }
	}

	#[inline(always)]
	fn check_bit(byte: u8, idx: u8) -> bool {
		if S {
			byte & (1u8 << idx) != 0
		} else {
			byte & (1u8 << idx) == 0
		}
	}
}

impl<'a, const S: bool> Iterator for IterFiltered<'a, S> {
	type Item = u16;

	fn next(&mut self) -> Option<Self::Item> {
		'outer: loop {
			let (idx, byte) = match self.cur_byte {
				Some(e) => e,
				None => *self.cur_byte.insert(self.upper.next()?),
			};

			// we expect most bits *not* to match
			if byte == Self::skip_clue() {
				debug_assert!(self.bit_pos == 0);
				self.cur_byte = None;
				continue;
			}

			loop {
				// start with the bit pos we have, maybe break early

				let test_bit_pos = Some((idx, self.bit_pos))
					.filter(|(_, p)| Self::check_bit(byte, *p));

				// increment bit position for next time
				let byte_done;
				self.bit_pos = match self.bit_pos + 1 {
					8 => {
						// new byte
						self.cur_byte = None;
						byte_done = true;
						0
					},
					new => {
						byte_done = false;
						new
					},
				};

				if test_bit_pos.is_some() { break 'outer test_bit_pos };
				if byte_done { continue 'outer; }
			};
		}.map(|(found_idx, found_bit)| (found_idx * 8) as u16 + found_bit as u16)
	}
}

pub type IterSet<'a> = IterFiltered<'a, true>;
pub type IterClear<'a> = IterFiltered<'a, false>;


#[cfg(test)]
mod test {
	use super::PerLineBits;

	#[test]
	fn get() {
		let mut sut = PerLineBits::new();
		assert_eq!(Some(false), sut.try_get(0));
		sut.store[0] = 1;
		assert_eq!(Some(true), sut.try_get(0));

		assert_eq!(Some(false), sut.try_get(7));
		sut.store[0] = 0x80;
		assert_eq!(Some(true), sut.try_get(7));

		assert_eq!(false, sut.get(9));
		sut.store[1] = 2;
		assert_eq!(true, sut.get(9));

		assert!(sut.try_get(0xff00).is_none());
		assert!(sut.try_get(u16::MAX).is_none());
	}

	#[test]
	fn get_mut() {
		let mut sut = PerLineBits::new();

		assert_eq!(0, sut.store[0]);
		let mut r#ref = sut.try_get_mut(3).unwrap();
		assert!(! r#ref.bit());
		r#ref.set();
		assert_eq!(1u8<<3, sut.store[0]);

		sut.store[0x20] = 0b1111_1111;
		sut.get_mut(0x104).clear();
		assert_eq!(0b1110_1111, sut.store[0x20]);

		sut.store[2] = 0b0000_0010;
		sut.get_mut(16).set_to(true);
		sut.get_mut(17).set_to(false);
		assert_eq!(0b0000_0001, sut.store[2]);

		assert!(sut.try_get_mut(0xff00).is_none());
		assert!(sut.try_get_mut(u16::MAX).is_none());
	}
}

#[cfg(test)]
mod test_iter {
	use super::*;

	#[test]
	fn iter_bits() {
		let mut upper = PerLineBits::new();
		upper.store[0] = 0b1010_0011;

		let mut sut = upper.iter();
		for expect in [
			Some(true), Some(true), Some(false), Some(false),
			Some(false), Some(true), Some(false), Some(true),
		] {
			assert_eq!(expect, sut.next());
		}
	}

	#[test]
	fn iter_set() {
		let mut upper = PerLineBits::new();
		upper.store[0] = 0b1101_0000;
		upper.store[1] = 0b0000_1111;
		upper.store[PerLineBits::BYTE_COUNT - 1] = 1;

		let mut sut = upper.iter_set();
		for expect in [4, 6, 7, 8, 9, 10, 11, 0xfef8] {
			assert_eq!(Some(expect), sut.next());
		}
		assert_eq!(None, sut.next());
	}

	#[test]
	fn iter_cleared() {
		let mut upper = PerLineBits::new();
		// pre-set all bits
		for byte in &mut *upper.store {
			*byte = 0xff;
		}
		upper.store[0] = 0b0111_1110;
		upper.store[50] = 0b1110_0111;

		let mut sut = upper.iter_clear();
		for expect in [0, 7, 403, 404] {
			assert_eq!(Some(expect), sut.next());
		}
		assert_eq!(None, sut.next());
	}
}

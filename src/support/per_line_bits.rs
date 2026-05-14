//! Bespoke bit array of a specific capacity.

use core::{iter, slice};
use std::fmt;

/// A packed array of bits, big enough to hold one bit per possible line number in a BASIC file
/// (`0..0xff00`).
///
/// The backing storage is automatically boxed.
pub struct PerLineBits {
	store: Box<[u8; Self::BYTE_COUNT]>,
}

impl fmt::Debug for PerLineBits {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "PerLineBits {{ store: {} bytes }}", Self::BYTE_COUNT)
	}
}

impl PerLineBits {
	const BYTE_COUNT: usize = 0xff00usize.div_ceil(8);

	/// Constructs a new object. All bits are initialised to `false`.
	pub fn new() -> Self {
		Self {
			store: Box::new([0u8; Self::BYTE_COUNT])
		}
	}

	/// Retrieves the value of a bit in the array.
	///
	/// Returns `None` if the index is outside the logical index range.
	pub fn try_get(&self, index: u16) -> Option<bool> {
		let (byte_idx, bit_mask) = Self::decompose(index);
		self.store.get(byte_idx as usize).map(|r| *r & bit_mask != 0)
	}

	/// Retrieves the value of a bit in the array.
	///
	/// # Panics
	///
	/// This function will panic if the index is out of range.
	pub fn get(&self, index: u16) -> bool {
		self.try_get(index).expect("index out of range")
	}

	/// Sets a bit in the array at the given index.
	pub fn set(&mut self, index: u16) {
		let (index, mask) = Self::decompose(index);
		self.store[usize::from(index)] |= mask;
	}

	/// Returns an iterator over **indexes** of all set bits in the array.
	pub fn iter_set(&self) -> IterSet<'_> {
		IterSet::new(self)
	}

	// decomposes a logical index into a byte index and bitmask
	fn decompose(index: u16) -> (u16, u8) {
		debug_assert!(Self::BYTE_COUNT < u16::MAX as usize);

		((index >> 3), 1 << (index & 0b111) as u8)
	}
}


/// Iterates over the indexes of all set bits.
pub struct IterSet<'a> {
	upper: iter::Enumerate<iter::Copied<slice::Iter<'a, u8>>>,
	cur_byte: Option<(usize, u8)>,
	bit_pos: u8,
}

impl fmt::Debug for IterSet<'_> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.debug_struct(stringify!(IterSet))
			.field("cur_byte", &IterSetPos(self))
			.field("bit_pos", &format_args!("1<<{}", self.bit_pos))
			.finish()
	}
}

/// Helper struct for `IterSet`'s [`Debug`](std::fmt::Debug) impl.
struct IterSetPos<'a>(&'a IterSet<'a>);
impl fmt::Debug for IterSetPos<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self.0.cur_byte {
			Some((idx, byte)) => write!(f, "{idx}/{byte:02x}"),
			None => f.write_str("None"),
		}
	}
}

impl<'a> IterSet<'a> {
	fn new(upper: &'a PerLineBits) -> Self {
		Self {
			upper: upper.store.iter().copied().enumerate(),
			cur_byte: None,
			bit_pos: 0,
		}
	}

	// if the byte is set to this, don't bother checking individual bits
	const SKIP_CLUE: u8 = 0x00;

	const fn check_bit(byte: u8, idx: u8) -> bool {
		byte & (1u8 << idx) != 0
	}
}

impl Iterator for IterSet<'_> {
	type Item = u16;

	fn next(&mut self) -> Option<Self::Item> {
		'outer: loop {
			let (idx, byte) = match self.cur_byte {
				Some(e) => e,
				None => *self.cur_byte.insert(self.upper.next()?),
			};

			// we expect most bits *not* to match
			if byte == Self::SKIP_CLUE {
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

				if test_bit_pos.is_some() { break 'outer test_bit_pos }
				if byte_done { continue 'outer; }
			}
		}.map(
			// `found_idx` is an index of a fixed-size array whose indexes can't exceed a u16
			#[allow(clippy::cast_possible_truncation)]
			|(found_idx, found_bit)| (found_idx * 8) as u16 + u16::from(found_bit)
		)
	}
}


#[cfg(test)]
mod test {
	use super::PerLineBits;

	#[test]
	fn get_mut() {
		let mut sut = PerLineBits::new();

		assert_eq!(0, sut.store[0]);
		sut.set(3);
		assert_eq!(1u8<<3, sut.store[0]);

		sut.store[2] = 0b0000_0010;
		sut.set(16);
		assert_eq!(0b0000_0011, sut.store[2]);
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
}

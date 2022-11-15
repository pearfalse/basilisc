use std::{mem::ManuallyDrop, ptr};

use arrayvec::ArrayVec;

pub(crate) trait ArrayVecExt<T> {
	/// Pops the first element from the front of the ArrayVec, shifting all others forward one
	/// position.
	fn pop_front(&mut self) -> Option<T>;

	/// Removes the first `x` elements from the front of the ArrayVec. The elements are not dropped.
	///
	/// # Panics
	///
	/// Panics if `x` exceeds the ArrayVec's length.
	fn remove_first(&mut self, x: usize);
}

impl<T, const N: usize> ArrayVecExt<T> for ArrayVec<T, N> {
	fn pop_front(&mut self) -> Option<T> {
		let new_len = self.len().checked_sub(1)?;

		// keep a non-drop copy of the element to pop
		let ret = ManuallyDrop::new(unsafe {
			ptr::read(self.first()? as *const T)
		});

		// reduce the length by 1, then move all elements up one place
		unsafe {
			self.set_len(new_len);
			ptr::copy(self.as_ptr().add(1), self.as_mut_ptr(), new_len);
		}

		Some(ManuallyDrop::into_inner(ret))
	}

	fn remove_first(&mut self, x: usize) {
		let old_len = self.len();
		let new_len = old_len.checked_sub(x as usize)
			.unwrap_or_else(|| panic!("couldn't remove {} elements from a {}-wide ArrayVec",
				x, old_len));
		unsafe {
			let data = self.as_mut_ptr();
			std::ptr::copy(data.add(x), data, new_len);
			self.set_len(new_len);
		}
	}

}


#[cfg(test)]
mod test_arrayvec_ext {
	use super::*;

	#[test]
	fn pop_front_with_integers() {
		let mut arr = ArrayVec::from([1u8, 2, 3, 4, 5]);

		assert_eq!(Some(1u8), arr.pop_front());
		assert_eq!([2,3,4,5].as_slice(), &*arr);
	}

	// asserts that calling drop glue happens correctly
	#[test]
	fn pop_front_check_drop() {
		use std::cell::Cell;

		let check_slots = Cell::new(0u32);

		#[derive(Debug)]
		struct CheckedDrop<'a> {
			slots: &'a Cell<u32>,
			mask: u32,
		}

		impl<'a> Drop for CheckedDrop<'a> {
			fn drop(&mut self) {
				// assert this one was not already dropped
				assert_eq!(0, self.slots.get() & self.mask);
				self.slots.set(self.slots.get() | self.mask);
			}
		}

		let mut arr = ArrayVec::<CheckedDrop<'_>, 32>::new();
		for i in 0..32 {
			arr.push(CheckedDrop {
				slots: &&check_slots,
				mask: 1 << i,
			});
		}

		assert_eq!(0, check_slots.get());
		for i in 0..32 {
			let got = arr.pop_front().unwrap();
			// assert they were pulled off in order
			assert_eq!(1 << i, got.mask);
			// `got` is dropped here
		}
		assert_eq!(u32::MAX, check_slots.get());

		assert!(arr.is_empty()); // should have pulled them all off by now
	}

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

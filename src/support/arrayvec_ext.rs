//! Container module for additional features added to `ArrayVec`.
//!
//! See [ArrayVecExt] for more.

use std::{mem::{ManuallyDrop, self, MaybeUninit}, ptr, slice};
use arrayvec::ArrayVec;

/// Extension trait for additional batch-removing methods needed on ArrayVec.
///
/// While only useful in practice on an element type of `u8`, this trait supports any arbitrary
/// type, including those that do not implement [`Copy`](std::marker::Copy).
///
/// [rust_Copy]: https://doc.rust-lang.org/std/marker/trait.Copy.html
pub(crate) trait ArrayVecExt<T> {
	/// Pops the first element from the front of the ArrayVec, shifting all others forward one
	/// position.
	fn pop_front(&mut self) -> Option<T>;

	/// Removes the first `x` items from the front of the ArrayVec.
	///
	/// # Panics
	///
	/// Panics if `x` exceeds the ArrayVec's length, or if any removed item's `Drop` implementation
	/// panics (which may cause other items to leak).
	fn remove_first(&mut self, x: usize);
}

impl<T, const N: usize> ArrayVecExt<T> for ArrayVec<T, N> {
	fn pop_front(&mut self) -> Option<T> {
		let new_len = self.len().checked_sub(1)?;

		// keep a non-drop copy of the element to pop
		let ret = ManuallyDrop::new(unsafe {
			ptr::read(self.first()? as *const T)
		});

		// reduce the length by 1, then move all items up one place
		unsafe {
			let dst = self.as_mut_ptr();
			let src = (dst as *const T).add(1);
			ptr::copy(src, dst, new_len);
			self.set_len(new_len);
		}

		Some(ManuallyDrop::into_inner(ret))
	}

	fn remove_first(&mut self, x: usize) {
		let must_drop = mem::needs_drop::<T>() && N > 0;

		let old_len = self.len();
		let new_len = old_len.checked_sub(x as usize)
			.unwrap_or_else(|| panic!("couldn't remove {} items from a {}-wide ArrayVec",
				x, old_len));

		let mut drop_sites = unsafe {
			// this is MaybeUninit::uninit_array, but usable on stable
			MaybeUninit::<[MaybeUninit<T>; N]>::uninit().assume_init()
		};
		if must_drop {
			// copy items to here for dropping later
			unsafe {
				// we go through a raw slice to keep miri happy
				let dst = slice::from_raw_parts_mut(drop_sites.as_mut_ptr(), x)
					as *mut [MaybeUninit<T>] as *mut T;
				ptr::copy_nonoverlapping(self.as_ptr(), dst, x);
			}
		}

		// copy other elements down
		unsafe {
			// pre-declare src and dst pointers to keep miri happy (it seems to dislike fetching
			// two pointers inline due to the side-by-side method calls to `&self` and `&mut self`)
			let dst = self.as_mut_ptr();
			let src = (dst as *const T).add(x);
			ptr::copy(src, dst, new_len);
			self.set_len(new_len);
		}

		if must_drop {
			// drop removed items
			for site in &mut drop_sites[..x] {
				unsafe {
					// SAFETY: was init'd if MUST_DROP, and if that's false, we don't even run this
					ptr::drop_in_place(site.as_mut_ptr());
				}
			}
		}
	}

}


#[cfg(test)]
mod test_arrayvec_ext {
	use super::*;

	use std::cell::Cell;

	#[derive(Debug)]
	struct CheckedDropSite {
		slots: Cell<u32>,
		next: Cell<u32>,
	}

	impl CheckedDropSite {
		#[inline]
		pub fn new() -> Self {
			Self {
				slots: Cell::new(0),
				next: Cell::new(1 << 0),
			}
		}

		#[inline]
		pub fn all_dropped(&self) -> bool { self.slots.get() == u32::MAX }

		#[inline]
		pub fn drop_mask(&self) -> u32 { self.slots.get() }

		pub fn next(&self) -> Option<CheckedDrop<'_>> {
			let mask = Some(self.next.get()).filter(|n| *n != 0)?;
			self.next.set(self.next.get() << 1);

			Some(CheckedDrop {
				slots: &self.slots,
				mask,
			})
		}
	}

	#[derive(Debug)]
	struct CheckedDrop<'a> {
		slots: &'a Cell<u32>,
		mask: u32,
	}

	impl<'a> CheckedDrop<'a> {
		#[inline]
		pub fn index(&self) -> u32 { self.mask.trailing_zeros() }

		#[doc(hidden)]
		fn _mark_dropped(&self) {
			// assert this one was not already dropped
			assert_eq!(0, self.slots.get() & self.mask,
				"slot {:08x} (index {}) was already dropped", self.mask, self.index());
			eprintln!("dropping index {}", self.index());
			self.slots.set(self.slots.get() | self.mask);
		}
	}

	impl<'a> Drop for CheckedDrop<'a> {
		fn drop(&mut self) {
			self._mark_dropped();
		}
	}

	#[test]
	fn pop_front_with_integers() {
		let mut arr = ArrayVec::from([1u8, 2, 3, 4, 5]);

		assert_eq!(Some(1u8), arr.pop_front());
		assert_eq!([2,3,4,5].as_slice(), &*arr);
	}

	// asserts that calling drop glue happens correctly
	#[test]
	fn pop_front_check_drop() {
		let checked_drops = CheckedDropSite::new();

		let mut arr = ArrayVec::<CheckedDrop<'_>, 32>::new();
		while let Some(cd) = checked_drops.next() {
			arr.push(cd);
		}

		for i in 0..32 {
			let got = arr.pop_front().unwrap();
			// assert they were pulled off in order
			assert_eq!(i, got.index());
			// `got` is dropped here
		}

		assert!(checked_drops.all_dropped());
		assert!(arr.is_empty()); // should have pulled them all off by now
	}

	#[test]
	fn remove_first() {
		let drop_checks = CheckedDropSite::new();
		let mut av: ArrayVec<(CheckedDrop<'_>, u8), 9> = ArrayVec::new();

		for i in 20..29 {
			av.push((
				drop_checks.next().unwrap(), i
			));
		}

		av.remove_first(3);
		assert_eq!(0b111, drop_checks.drop_mask());
		assert_eq!(
			&[23, 24, 25, 26, 27, 28],
			&*av.into_iter().map(|(_, i)| i).collect::<ArrayVec<u8, 6>>()
		);
	}
}

use std::marker::PhantomData;

use super::UnnumberedLine;

#[allow(unstable_name_collisions)]
use sptr::Strict;

#[derive(Debug)]
pub(super) struct Gap<'a> {
	pub before: Option<u16>,
	pub after: Option<u16>,
	pub lines: &'a mut [UnnumberedLine],
}

#[derive(Debug)]
pub(super) struct FindGaps<'a> {
	start: *mut UnnumberedLine,
	end: *mut UnnumberedLine,
	_lifetime: PhantomData<&'a mut UnnumberedLine>,
}

impl<'a> FindGaps<'a> {
	pub fn within(lines: &'a mut [UnnumberedLine]) -> Self {
		let start = lines.as_mut_ptr();
		Self {
			start,
			end: unsafe { start.add(lines.len()) },
			_lifetime: PhantomData,
		}
	}

	fn sp_addr(&self, addr: &UnnumberedLine) -> *mut UnnumberedLine {
		self.start.with_addr((addr as *const UnnumberedLine).addr())
	}
}

impl<'a> Iterator for FindGaps<'a> {
	type Item = Gap<'a>;

	fn next(&mut self) -> Option<Self::Item> {
		use std::ptr;

		let mut search = unsafe {
			// SAFETY: shared borrow starts here; to be split before &mut refs
			&*ptr::slice_from_raw_parts(self.start,
				self.end.offset_from(self.start) as usize)
		};
		let (before, slice_start) = loop {
			match search {
				[a, ..] if a.line_number.is_none()
				// start of slice is without line number
				// should only happen on first iteration
				=> break (None, self.sp_addr(a)),

				[a, b, ..] if a.line_number.is_some() && b.line_number.is_none()
				// we've found the boundary
				=> {
					search = &search[1..];
					break (a.line_number, self.sp_addr(b))
				},

				[]
				// no more nodes to iterate
				=> return None,

				[_, rest @ ..]
				// pop front node and keep doing
				=> {
					debug_assert!(!search.is_empty());
					search = rest;
				}
			}

		};

		let mut count = 0usize;
		let (after, new_start) = loop {
			match search {
				[a, rest @ ..] if a.line_number.is_none()
				// keep searching
				=> {
					count += 1;
					search = rest;
					continue;
				},

				[a, ..]
				// next line number found
				=> {
					debug_assert!(a.line_number.is_some());
					break (a.line_number, self.sp_addr(a));
				},

				[]
				//end
				=> break (None, self.end),
			}
		};

		// restrict ptr slice to other elements we haven't scanned yet
		// this makes it safe to then mutably downcast the pointers we saved
		self.start = new_start;

		Some(Gap {
			before, after, lines: unsafe {
				// SAFETY: there is no other active &'a mut borrow, and
				// no overlap between self.lines and our new slice
				debug_assert!(slice_start.add(count) <= new_start);

				// TODO: miri is unhappy with this on the stable path, because the address
				// technically comes from a *const UnnumberedLine that we cast, but with strict
				// provenance not available on stable, we don't have a lot of choice here
				&mut *std::ptr::slice_from_raw_parts_mut(slice_start, count)
			}
		})
	}
}

#[cfg(test)]
mod tests {
	use arrayvec::ArrayVec;

	use super::*;

	fn make(count: u8) -> (Vec<UnnumberedLine>, *const UnnumberedLine) {
		use std::io::Write;
		let mut write_buf = ArrayVec::<u8, 2>::new();
		let vec = (0..count).map(move |idx| {
			write_buf.clear();
			write!(&mut write_buf, "{:02x}", idx).unwrap();
			UnnumberedLine {
				line_number: None,
				contents: (&*write_buf).to_owned().into_boxed_slice(),
			}
		}).collect::<Vec<_>>();

		let start = vec.as_ptr();
		(vec, start)
	}

	#[test]
	fn no_given_lines() {
		let (mut stack, start) = make(5);

		let mut gaps = FindGaps::within(&mut *stack);
		let next = gaps.next().unwrap();
		assert_eq!(None, next.before);
		assert_eq!(None, next.after);
		assert_eq!(start, next.lines.as_ptr());
		assert_eq!(5, next.lines.len());

		assert!(gaps.next().is_none());
	}

	#[test]
	fn add_third_of_five() {
		let (mut stack, start) = make(5);

		stack[0].line_number = Some(10);
		stack[1].line_number = Some(20);
		stack[3].line_number = Some(40);
		stack[4].line_number = Some(50);

		let mut gaps = FindGaps::within(&mut stack);
		let next = gaps.next().unwrap();
		assert_eq!(Some(20), next.before); // gap is after line 20
		assert_eq!(Some(40), next.after); // gap is before line 40
		assert_eq!(unsafe { start.add(2) }, next.lines.as_ptr()); // gap is [2..=2]
		assert_eq!(1, next.lines.len()); // ditto

		assert!(gaps.next().is_none());
	}

	#[test]
	fn goto_100() {
		let (mut stack, start) = make(7);

		stack[3].line_number = Some(100);

		let mut gaps = FindGaps::within(&mut stack);
		let next = gaps.next().unwrap();
		assert_eq!(None, next.before);
		assert_eq!(Some(100), next.after);
		assert_eq!(start, next.lines.as_ptr());
		assert_eq!(3, next.lines.len());

		let next = gaps.next().unwrap();
		assert_eq!(Some(100), next.before);
		assert_eq!(None, next.after);
		assert_eq!(unsafe { start.add(4) }, next.lines.as_ptr());
		assert_eq!(3, next.lines.len());

		assert!(gaps.next().is_none());
	}
}

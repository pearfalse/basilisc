use std::marker::PhantomData;

use super::UnnumberedLine;

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
				=> break (None, a as *const _ as *mut UnnumberedLine),

				[a, b, ..] if a.line_number.is_some() && b.line_number.is_none()
				// we've found the boundary
				=> {
					search = &search[1..];
					break (a.line_number, b as *const _ as *mut UnnumberedLine)
				},

				[]
				// no more nodes to iterate
				=> return None,
				_

				// pop front node and keep doing
				=> {
					debug_assert!(!search.is_empty());
					search = &search[1..];
				}
			}
		};

		let mut count = 0usize;
		let (after, new_start) = loop {
			match search {
				[a, ..] if a.line_number.is_none()
				// keep searching
				=> {
					count += 1;
					continue;
				},

				[a, ..]
				// next line number found
				=> {
					debug_assert!(a.line_number.is_some());
					break (a.line_number, a as *const _ as *mut UnnumberedLine);
				},

				[]
				//end
				=> break (None, <&mut [UnnumberedLine]>::default().as_mut_ptr()),
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
				&mut *std::ptr::slice_from_raw_parts_mut(slice_start, count)
			}
		})
	}
}

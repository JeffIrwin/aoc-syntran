
! The equivalent syntran program runs in 3 minutes.  This runs in 2 seconds
! compiled in the debug profile

module mod_
	use iso_fortran_env
	implicit none
contains

function read_line(iu, io) result(s)
	! Read a whole line (any length) as string s from file unit iu
	!
	! There are better ways to do this without count_chars, c.f. the syntran
	! interpreter source

	character(len = :), allocatable :: s
	integer :: io, iu, ns

	ns = count_chars(iu)
	allocate(character(len = ns) :: s)
	read(iu, '(a)', iostat = io) s
end function read_line

!===============================================================================

integer function count_chars(iu) result(n)
	! Count number of characters n in the next line of a file unit iu
	character :: c
	integer :: io, iu

	n = 0
	do
		read(iu, '(a)', advance = 'no', iostat = io) c
		if (io == iostat_end) exit
		if (io == iostat_eor) exit
		n = n + 1
	end do
	backspace(iu)

end function count_chars

!===============================================================================

integer function count_str_match(str_, char_) result(n)
	! Count the number `n` of characters `char_` in string `str_`
	character(len = *), intent(in) :: str_
	character, intent(in) :: char_
	!********
	integer :: i
	n = 0
	do i = 1, len(str_)
		if (str_(i:i) == char_) n = n + 1
	end do
end function count_str_match

!===============================================================================

subroutine print_mat_f32(msg, a)
	! Pretty-print a matrix
	!
	! Note that this is transposed compared to Fortran's internal memory layout
	character(len = *), intent(in) :: msg
	real, intent(in) :: a(:,:)
	!********
	integer :: i, j, m, n
	m = size(a,1)
	n = size(a,2)
	print "(a)", " " // msg
	do i = 1, m
		do j = 1, n
			write(*, "(es11.3)", advance = "no") a(i,j)
		end do
		print *, ""
	end do

end subroutine print_mat_f32

!===============================================================================

subroutine print_mat_i32(msg, a)
	! Pretty-print a matrix
	!
	! Note that this is transposed compared to Fortran's internal memory layout
	character(len = *), intent(in) :: msg
	integer, intent(in) :: a(:,:)
	!********
	integer :: i, j, m, n
	m = size(a,1)
	n = size(a,2)
	print "(a)", " " // msg
	do i = 1, m
		do j = 1, n
			write(*, "(i6)", advance = "no") a(i,j)
		end do
		print *, ""
	end do

end subroutine print_mat_i32

!===============================================================================

function solve_ilp(a, b) result(iopt)
	integer, intent(in) :: a(:,:), b(:)
	integer, allocatable :: iopt(:)
	!********
	integer :: h, i, j, k, m, n, nfree, imax, i1(1), i0
	integer, allocatable :: inonz(:), ifree(:), imaxes(:), combos(:)
	logical :: is_valid
	real :: amax, f, fopt, sumx
	real, allocatable :: t(:,:), xopt(:), x(:)

	m = size(a,1)
	n = size(a,2)
	!print *, "m, n = ", m, n

	! Form augmented matrix tableau
	allocate(t(m, n+1))
	t = 0.0
	t(:, 1: n  ) = a
	t(:,    n+1) = b

	! Gaussian elimination to reduced row echelon form
	!
	! See part2.syntran or main.syntran for marginally more comments on the same
	! algorithm
	allocate(inonz(m))
	inonz = -1
	allocate(ifree(n))
	ifree = 0
	nfree = 0
	h = 1
	k = 1
	do while (h <= m .and. k <= n)
		! Find pivot
		i1 = maxloc(abs(t(h:m, k)))
		print *, "i1 = ", i1
		imax = i1(1) + h - 1  ! almost easier to just scan manually, like below
		amax = -1.0
		if (imax >= 1) amax = abs(t(imax, k))
		!imax = h
		!amax = abs(t(h,k))
		!do i = h+1, m-1
		!	if (abs(t(i,k)) > amax) then
		!		amax = abs(t(i,k))
		!		imax = i
		!	end if
		!end do

		!print *, "imax, amax = ", imax, amax

		if (amax < 0.0001) then
			nfree = nfree + 1
			ifree(nfree) = k
			k = k + 1
		else
			inonz(h) = k
			!print *, "swapping ", h, imax
			if (h /= imax) t([h, imax], :) = t([imax, h], :)

			do i = 1, m  ! RREF
				if (i == h) cycle
				f = t(i,k) / t(h,k)
				print *, "f = ", f
				t(i,k) = 0.0
				t(i, k+1: n+1) = t(i, k+1: n+1) - t(h, k+1: n+1) * f
			end do

			h = h + 1
			k = k + 1
		end if
	end do
	print *, "inonz = ", inonz
	!call print_mat_f32("t ref = ", t)

	! Get the rest of the free vars
	do k = m, n
		if (any(ifree == k)) cycle
		if (any(inonz == k)) cycle
		nfree = nfree + 1
		ifree(nfree) = k
	end do
	ifree = ifree(1: nfree)
	print *, "nfree = ", nfree
	!print *, "ifree = ", ifree

	allocate(imaxes(n))
	imaxes = 0
	do j = 1, n
	do i = 1, m
		if (a(i,j) /= 0) then
			imaxes(j) = max(imaxes(j), b(i))
		end if
	end do
	end do
	!print *, "imaxes (full) = ", imaxes

	allocate(xopt(n))
	xopt = 0.0
	fopt = huge(fopt)
	!print *, "fopt = ", fopt

	! Iterate over possible value combinations of free vars
	imaxes = imaxes(ifree)
	allocate(combos(nfree))
	combos = 0
	!print *, "imaxes = ", imaxes

	i0 = -1
	do i = m, 1, -1
		if (inonz(i) >= 1) then
			i0 = i
			exit
		end if
	end do
	!print *, "i0 = ", i0

	! TODO: zeros(), ones(), etc. fns like matlab would be nice here. I have
	! most of this implemented with nice overloads in my numerical-analysis repo
	allocate(x(n))
	x = 0.0
	do
		sumx = sum(1.0 * combos)
		!print *, "combos = ", combos
		if (sumx < fopt) then
			is_valid = .true.
			x = 0.0
			x(ifree) = combos

			! Back substitute to solve for the other vars
			do i = i0, 1, -1
				k = inonz(i)
				!print *, "k = ", k
				x(k) = t(i, n+1)
				x(k) = x(k) - dot_product(t(i, ifree), x(ifree))
				x(k) = x(k) / t(i,k)

				is_valid = x(k) > -0.0001
				if (.not. is_valid) exit

				sumx = sumx + x(k)
				is_valid = sumx < fopt
				if (.not. is_valid) exit

				is_valid = is_int(x(k))
				if (.not. is_valid) exit
			end do

			if (is_valid) then
				fopt = sumx
				print *, "fopt = ", fopt
				xopt = x
			end if
		end if
		if (.not. next_combo(combos, imaxes)) exit
	end do
	iopt = nint(xopt)

end function solve_ilp

!===============================================================================

logical function is_int(x)
	real, intent(in) :: x
	is_int = abs(x - nint(x)) < 0.0001
end function is_int

!===============================================================================

logical function next_combo(c, n)
	! Bignum += 1 algo for number in array c with mixed radix
	integer, intent(inout) :: c(:)
	integer, intent(in) :: n(:)
	!********
	integer :: i, nc

	nc = size(c)
	next_combo = .false.
	if (nc == 0) return

	! Find first digit less than n-1
	i = 1
	do while (c(i) == n(i)-1)
		c(i) = 0
		i = i + 1
		if (i > nc) return
	end do
	c(i) = c(i) + 1
	next_combo = .true.

end function next_combo

!===============================================================================

end module mod_

program main
	use mod_
	implicit none

	integer :: iu, io, sum_, i0, i1, num_jolts, num_buttons, n, ib
	integer, allocatable :: jolts_goal(:), ibuttons(:), buttons(:,:), iopt(:)
	character(len = :), allocatable :: str_, jolts_str, button_str

	print *, "starting main.f90"
	sum_ = 0

	open(newunit = iu, file = "input.txt")
	!open(newunit = iu, file = "test-input.txt")
	do
		str_ = read_line(iu, io)
		if (io /= 0) exit
		print *, "str_ = ", str_

		! TODO: I need to port my str split_() fn to Fortran
		i0 = scan(str_, "{") + 1
		i1 = scan(str_, "}") - 1
		jolts_str = str_(i0: i1)

		num_jolts = count_str_match(jolts_str, ",") + 1
		allocate(jolts_goal(num_jolts))
		read(jolts_str, *) jolts_goal
		print *, "jolts_goal = ", jolts_goal

		num_buttons = count_str_match(str_, "(")
		allocate(buttons(0:num_jolts-1, 0:num_buttons-1))
		buttons = 0

		! Parse buttons (split would really help here)
		i0 = 0
		ib = 0
		do
			i0 = i0 + 1
			if (i0 > len(str_)) exit
			if (str_(i0:i0) /= "(") cycle

			i1 = i0 + 1
			do while (str_(i1:i1) /= ")")
				i1 = i1 + 1
			end do

			i0 = i0 + 1
			i1 = i1 - 1
			button_str = str_(i0: i1)

			n = count_str_match(button_str, ",") + 1
			allocate(ibuttons(n))
			read(button_str, *) ibuttons

			buttons(ibuttons, ib) = 1
			ib = ib + 1

			deallocate(ibuttons)
		end do
		call print_mat_i32("buttons = ", buttons)

		iopt = solve_ilp(buttons, jolts_goal)
		sum_ = sum_ + sum(iopt)
		!print *, "Line: ", iline
		print *, "Optimal solution = ", iopt
		print *, "Optimal value    = ", sum(iopt)

		!********
		deallocate(jolts_goal)
		deallocate(buttons)
	end do
	close(iu)

	print *, "part 2 = ", sum_
	print *, "ending main.f90"

end program main


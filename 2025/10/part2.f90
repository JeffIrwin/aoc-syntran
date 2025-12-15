
! The equivalent syntran program runs in 3 minutes.  This runs in 2 seconds
! compiled in the debug profile

module mod_

	use iso_fortran_env
	implicit none

contains

function readline(iu, io) result(s)
	! Read a whole line (any length) as string s from file unit iu
	!
	! There are better ways to do this without countchars, c.f. the syntran
	! interpreter source

	character(len = :), allocatable :: s
	integer :: io, iu, ns

	ns = countchars(iu)
	!print *, 'ns = ', ns
	allocate(character(len = ns) :: s)
	read(iu, '(a)', iostat = io) s
	!print *, 's = ', s

end function readline

!===============================================================================

integer function countchars(iu) result(n)
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

end function countchars

!===============================================================================

integer function count_str_match(str_, char_) result(n)
	! Count the number `n` of characters `char_` in string `str_`
	character(len = *), intent(in) :: str_
	character, intent(in) :: char_
	!********
	integer :: i
	n = 0
	do i = 1, len(str_)
		!print *, "str_ i = ", str_(i:i)
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
	m = ubound(a,1)-1
	n = ubound(a,2)-1
	!print *, "m, n = ", m, n
	print "(a)", " " // msg
	do i = lbound(a,1), m+1
		do j = lbound(a,2), n+1
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
	m = ubound(a,1)-1
	n = ubound(a,2)-1
	!print *, "m, n = ", m, n
	print "(a)", " " // msg
	do i = lbound(a,1), m+1
		do j = lbound(a,2), n+1
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

	!println("a init = ", transpose_(a)); // transpose for sane display
	!call print_mat_i32("a init = ", a)
	!let m = i32(size(a,0));
	!let n = i32(size(a,1));

	!m = ubound(a,1)-1
	!n = ubound(a,2)-1
	m = ubound(a,1)
	n = ubound(a,2)
	print *, "m, n = ", m, n

	! Form augmented matrix tableau
	allocate(t(0:m-1, 0:n))
	t = 0.0
	!let t = [0.0'f32; m, n+1];
	!t[0:m, 0:n] = a;
	!t[0:m, n] = b;
	t(:, 0:n-1) = a
	t(:,   n  ) = b
	!print *, "b = ", b
	!call print_mat_f32("t = ", t)

	! Gaussian elimination to (reduced?) row echelon form
	!let inonz = [-1; m];  // list of first (left-most) non-zero index in each row after reduction
	allocate(inonz(0:m-1))
	inonz = -1
	!let ifree = [0; n];   // list of free variables
	allocate(ifree(0:n-1))
	ifree = 0
	nfree = 0
	!let nfree = 0;
	!let h = 0;
	!let k = 0;
	h = 0
	k = 0
	!while h < m and k < n
	do while (h < m .and. k < n)
	!{
		! Find pivot

		i1 = maxloc(abs(t(h:m-1, k)))
		print *, "i1 = ", i1
		imax = i1(1) + h - 1  ! almost easier to just scan manually
		amax = -1.0
		if (imax >= 0) amax = abs(t(imax, k))

		!imax = h
		!amax = abs(t(h,k))
		!do i = h+1, m-1
		!	if (abs(t(i,k)) > amax) then
		!		amax = abs(t(i,k))
		!		imax = i
		!	end if
		!end do

		print *, "imax, amax = ", imax, amax

	!	let imax = h;
	!	let amax = abs(t[h,k]);
	!	for i in [h+1: m]
	!		if abs(t[i,k]) > amax
	!		{
	!			amax = abs(t[i,k]);
	!			imax = i;
	!		}
	!	//println("imax, amax = ", imax, amax);

		if (amax < 0.0001) then
			ifree(nfree) = k
			nfree = nfree + 1
			k = k + 1
		else
			inonz(h) = k
			print *, "swapping ", h, imax
			if (h /= imax) t([h, imax], :) = t([imax, h], :)
			!call print_mat_f32("t swapped = ", t)

			do i = 0, m-1  ! RREF
				if (i == h) cycle
				f = t(i,k) / t(h,k)
				print *, "f = ", f
				t(i,k) = 0.0
				t(i, k+1: n) = t(i, k+1: n) - t(h, k+1: n) * f
				!do j = k+1, n
				!	t(i,j) = t(i,j) - t(h,j) * f
				!	!t[i, k+1: n+1] -= t[h, k+1: n+1] * f;
				!end do
			end do
			!call print_mat_f32("t reduced = ", t)

			h = h + 1
			k = k + 1
		end if
		print *, ""
	!	if abs(t[imax,k]) < 0.0001'f32
	!	{
	!		//println("skipped piv k = ", k);
	!		ifree[nfree] = k;
	!		nfree += 1;
	!		k += 1;
	!	}
	!	else
	!	{
	!		//println("swapping");
	!		//println("imax, h = ", [imax, h]);
	!		inonz[h] = k;
	!		if (h != imax) t[[h, imax], :] = t[[imax, h], :];  // swap rows h and imax. TODO: fix interpretter

	!		for i in [0: m]  // RREF
	!		{
	!			if (i == h) continue;
	!			let f = t[i,k] / t[h,k];
	!			t[i,k] = 0.0'f32;
	!			t[i, k+1: n+1] -= t[h, k+1: n+1] * f;
	!		}
	!		h += 1;
	!		k += 1;
	!	}
	!}
	end do
	!//println("t ref = ", transpose_f32(t));
	!println("inonz = ", inonz);

	!call print_mat_f32("t rref = ", t)
	print *, "inonz = ", inonz

	! Get the rest of the free vars
	!for k in [m: n]
	!{
	!	if (any(ifree == k)) continue;
	!	if (any(inonz == k)) continue;
	!	ifree[nfree] = k;
	!	nfree += 1;
	!}
	!ifree = ifree[0: nfree];  // trim
	do k = m, n-1
		if (any(ifree == k)) cycle
		if (any(inonz == k)) cycle
		ifree(nfree) = k
		nfree = nfree + 1
	end do
	ifree = ifree(0: nfree-1)
	!println("nfree = ", nfree);
	!println("ifree = ", ifree);
	print *, "nfree = ", nfree
	print *, "ifree = ", ifree

	!let imaxes = [0; n];
	!for j in [0: n]
	!for i in [0: m]
	!	if a[i,j] != 0
	!		imaxes[j] = max(imaxes[j], b[i]);  // upper bound of search space
	!//println("imaxes (full) = ", imaxes);
	!//println("m, n, max inonz = ", [m, n, maxval(inonz)]);
	allocate(imaxes(0: n-1))
	imaxes = 0
	do j = 0, n-1
	do i = 0, m-1
		if (a(i+1,j+1) /= 0) then
			imaxes(j) = max(imaxes(j), b(i+1))
		end if
	end do
	end do
	print *, "imaxes (full) = ", imaxes

	!let xopt = [0.0'f32; n];
	!let fopt = 1.e20'f32;
	allocate(xopt(0: n-1))
	xopt = 0.0
	fopt = huge(fopt)
	!print *, "fopt = ", fopt

	! Iterate over possible value combinations of free vars
	imaxes = imaxes(ifree)
	allocate(combos(nfree))
	combos = 0
	!imaxes = imaxes[ifree];  // TODO: interpretter breaks on some empty ifree array like this in debug
	!let combos = [0; nfree];
	!println("imaxes = ", imaxes);
	print *, "imaxes = ", imaxes

	!// I guess you could also swap columns instead of using `inonz`
	!let i0 = -1;
	!for i in [m-1: -1: -1]
	!	if inonz[i] >= 0
	!	{
	!		i0 = i; // max non-free var
	!		break;
	!	}
	i0 = -1
	do i = m-1, 0, -1
		if (inonz(i) >= 0) then
			i0 = i
			exit
		end if
	end do

	allocate(x(0: n-1))
	x = 0.0
	!let x = [0.0'f32; n];
	!println("imaxes = ", imaxes, " (perms = ", product(imaxes), ")");
	!while true
	!{
	do
	!	let sumx = sum(1.0'f32 * combos);  // initialize sum to free vars only
	!	if sumx < fopt  // fast non-optimal check
		sumx = sum(1.0 * combos)
		!print *, "combos = ", combos

		if (sumx < fopt) then
	!	{
	!		let is_valid = true;
	!		x = 0.0'f32;
	!		x[ifree] = combos;
	!		//println("combos = ", combos);
	!		//println("x[ifree] = ", x[ifree]);
			is_valid = .true.
			x = 0.0
			x(ifree) = combos

			! Back substitute to solve for the other vars
			do i = i0, 0, -1
	!		for i in [i0: -1: -1]
	!		{
	!			let k = inonz[i];
	!			x[k] = t[i,n];
	!			x[k] -= dot(t[i, ifree], x[ifree]);  // rref saves math here, shorter sparse dot prod dims
	!			x[k] /= t[i,k];
				k = inonz(i)
				x(k) = t(i,n)
				x(k) = x(k) - dot_product(t(i, ifree), x(ifree))
				x(k) = x(k) / t(i,k)

	!			is_valid = x[k] > -0.0001;
	!			if (not is_valid) break;
				is_valid = x(k) > -0.0001
				if (.not. is_valid) exit

	!			sumx += x[k];
	!			is_valid = sumx < fopt;
	!			if (not is_valid) break;
				sumx = sumx + x(k)
				is_valid = sumx < fopt
				if (.not. is_valid) exit

	!			is_valid = is_int(x[k]);
	!			if (not is_valid) break;
				is_valid = is_int(x(k))
				if (.not. is_valid) exit
	!		}
			end do

	!		if is_valid
	!		{
	!			fopt = sumx;
	!			println("fopt = ", fopt);
	!			xopt = x;
	!		}
			if (is_valid) then
				fopt = sumx
				print *, "fopt = ", fopt
				xopt = x
			end if
	!	}
		end if
		if (.not. next_combo(combos, imaxes)) exit
	!	if (not next_combo(&combos, &imaxes)) break;
	end do
	!}
	!println("Optimal solution: ", xopt);
	!println("sum(xopt) = ", sum(xopt));

	!let iopt = i32(xopt);
	!for i in [0: n]
	!	if iopt[i] < xopt[i] - 0.5'f32
	!		iopt[i] += 1;  // round
	!return iopt;
	iopt = nint(xopt)

end function solve_ilp

!===============================================================================

logical function is_int(x)
	real, intent(in) :: x
	is_int = abs(x - nint(x)) < 0.0001
end function is_int

!===============================================================================

!fn next_combo(c: &[i32; :], n: &[i32; :]): bool
logical function next_combo(c, n)
	! Bignum += 1 algo for number in array c with mixed radix
	integer, intent(inout) :: c(:)
	integer, intent(in) :: n(:)
	!********
	integer :: i, nc
!	let nc = size(c,0);
!	if (nc == 0) return false;

	nc = size(c)
	next_combo = .false.
	if (nc == 0) return

!	// Find first digit less than n-1
!	let i = 0;
!	while c[i] == n[i]-1
!	{
!		c[i] = 0;
!		i += 1;
!		if (i == nc) return false;
!	}
	i = 1
	do while (c(i) == n(i)-1)
		c(i) = 0
		i = i + 1
		if (i > nc) return
	end do
	c(i) = c(i) + 1
	next_combo = .true.
!	c[i] += 1;
!	return true;

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
		str_ = readline(iu, io)
		if (io /= 0) exit
		print *, "str_ = ", str_

		! TODO: I need to port my str split_() fn to Fortran
		i0 = scan(str_, "{") + 1
		i1 = scan(str_, "}") - 1
		print *, "i0, i1 = ", i0, i1
		jolts_str = str_(i0: i1)
		print *, "jolts_str = ", jolts_str

		num_jolts = count_str_match(jolts_str, ",") + 1
		print *, "num_jolts = ", num_jolts
		allocate(jolts_goal(num_jolts))
		read(jolts_str, *) jolts_goal
		print *, "jolts_goal = ", jolts_goal

		num_buttons = count_str_match(str_, "(")
		print *, "num_buttons = ", num_buttons

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
			print *, "i0, i1 = ", i0, i1

			i0 = i0 + 1
			i1 = i1 - 1
			button_str = str_(i0: i1)
			print *, "button_str = ", button_str

			n = count_str_match(button_str, ",") + 1
			allocate(ibuttons(n))
			read(button_str, *) ibuttons
			print *, "ibuttons = ", ibuttons

			!buttons[parse_i32_delim(but_str, "(,)"), ib] = 1;
			buttons(ibuttons, ib) = 1
			ib = ib + 1

			deallocate(ibuttons)
		end do

		!print *, "buttons = ", buttons
		call print_mat_i32("buttons = ", buttons)

		!let iopt = solve_ilp(buttons, jolts_goal);
		!sum_ += sum(iopt);
		!println("Line: ", iline);
		!println("Optimal solution: ", iopt);
		!println("Optimal value: ", sum(iopt));
		!println(" ");

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


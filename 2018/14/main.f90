
program main

	integer(kind = 1), allocatable :: in_vec(:), v(:), v0, v1, sum_

	integer :: nin, nv, CAP, i0, i1, ans_

	print *, "starting main.f90"

	!in_vec = int([5, 9, 4, 1, 4], 1)
	in_vec = int([5, 0, 9, 6, 7, 1], 1)
	!in_vec = int([2, 8, 1, 0, 8, 6, 2, 2, 1, 1], 1)

	print *, "in_vec = ", in_vec
	nin = size(in_vec)

	CAP = 1024 * 1024 * 600

	allocate(v(0: CAP - 1))
	v = 0
	v(0: 1) = int([3, 7], 1)
	nv = 2

	! Elf positions
	i0 = 0
	i1 = 1

	do while (.true.)
		v0 = v(i0)
		v1 = v(i1)
		sum_ = v0 + v1

		if (sum_ < 10) then
			v(nv) = sum_
			nv = nv + 1
		else
			v(nv) = 1
			!v(nv+1) = mod(sum_, int(10,1))
			v(nv+1) = sum_ - int(10,1)
			nv = nv + 2
		end if

		! Step forward
		i0 = mod(i0 + v0 + 1, nv)
		i1 = mod(i1 + v1 + 1, nv)

		if (nv >= nin) then
			if (all(in_vec == v(nv - nin: nv-1))) then
				exit
			end if
		end if

		if (nv >= nin + 1) then
			! I lost so long missing this possibility
			if (all(in_vec == v(nv - nin - 1: nv-2))) then
				nv = nv - 1
				exit
			end if
		end if
	end do
	ans_ = nv - nin;

	print *, "part 2 = ", ans_
	print *, "ending main.f90"

end program main


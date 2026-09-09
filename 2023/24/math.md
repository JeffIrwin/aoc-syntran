
## Part 2

Find position vector `p` and velocity `v` such that an intersection occurs at time
`t0` with one stone, `t1` with a second stone, and `t2` with a third:

```
p + t0*v = q0 + t0*w0
p + t1*v = q1 + t1*w1
p + t2*v = q2 + t2*w2
```

where `q0` and `w0` are the known position and velocity of the first stone, `q1` and
`w1` for the next stone, etc.

Expand the vector equations above into scalars of each `x`, `y`, and `z` component:

```
px + t0*vx = qx0 + t0*wx0
py + t0*vy = qy0 + t0*wy0
pz + t0*vz = qz0 + t0*wz0
px + t1*vx = qx1 + t1*wx1
py + t1*vy = qy1 + t1*wy1
pz + t1*vz = qz1 + t1*wz1
px + t2*vx = qx2 + t2*wx2
py + t2*vy = qy2 + t2*wy2
pz + t2*vz = qz2 + t2*wz2
```

These are 9 equations with 9 unknowns:
- `px, py, pz`
- `vx, vy, vz`
- `t0, t1, t2`

All `q*` and `w*` terms are given knowns from the input data

Unfortunately this is a non-linear system: unknown `t*` terms are multiplied by
unknown `v` terms

Rearrange:

```
p - q0 + t0*v - t0*w0 = 0
p - q1 + t1*v - t1*w1 = 0
p - q2 + t2*v - t2*w2 = 0
```

Take derivatives with respect to `p`, `v`, and `t` to obtain a stiffness matrix for Newton-Raphson

d/dp:
```
1
1
1
```

d/dv
```
t0
t1
t2
```

d/dt:
```
v - w0
v - w1
v - w2
```

Equation ordering convention:

```
px + t0*vx - qx0 - t0*wx0 = 0
py + t0*vy - qy0 - t0*wy0 = 0
pz + t0*vz - qz0 - t0*wz0 = 0

px + t1*vx - qx1 - t1*wx1 = 0
py + t1*vy - qy1 - t1*wy1 = 0
pz + t1*vz - qz1 - t1*wz1 = 0

px + t2*vx - qx2 - t2*wx2 = 0
py + t2*vy - qy2 - t2*wy2 = 0
pz + t2*vz - qz2 - t2*wz2 = 0
```

Unknown ordering convention:
- `px, py, pz, vx, vy, vz, t0, t1, t2`

Stiffness matrix:

```
[
	1, 0, 0,   t0,  0,  0,     vx-wx0,      0,      0
	0, 1, 0,   0 , t0,  0,     vy-wy0,      0,      0
	0, 0, 1,   0 ,  0, t0,     vz-wz0,      0,      0
	
	1, 0, 0,   t1,  0,  0,          0, vx-wx1,      0
	0, 1, 0,    0, t1,  0,          0, vy-wy1,      0
	0, 0, 1,    0,  0, t1,          0, vz-wz1,      0
	
	1, 0, 0,   t2,  0,  0,          0,      0, vx-wx2
	0, 1, 0,    0, t2,  0,          0,      0, vy-wy2
	0, 0, 1,    0,  0, t2,          0,      0, vz-wz2
]
```

Expect that this stiffness matrix can be singular for degenarate cases if the
first two hailstones are parallel. Iterate over triplets of hailstones until a
non-singular system is found

You are free to order equations or unknowns differently. You will simply get a
different row/column permutation of my stiffness matrix


#!/usr/bin/env bash

set -exu

mkdir -p build

# Debug
gfortran -o part2 part2.f90 -fbounds-check -Wall -Wextra -Wno-tabs -fbacktrace

## Release
#gfortran -o part2 part2.f90 -O3 -Wall -Wextra -Wno-tabs

# cp for nvim linting nonsense
cp *.mod build

time ./part2


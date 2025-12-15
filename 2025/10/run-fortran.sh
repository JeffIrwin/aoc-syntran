#!/usr/bin/env bash

set -exu

mkdir -p build
gfortran -o part2 part2.f90 -fbounds-check -Wall -Wextra -Wno-tabs -fbacktrace

# cp for nvim linting nonsense
cp *.mod build

./part2


#!/usr/bin/env bash

# Run one syntran program, either by interpreting it or, if SYNTRAN_TRANSPILE is
# "true", by transpiling to Fortran, compiling with gfortran, and running the
# resulting binary
#
# Usage:  run-syntran.sh main.syntran [-- <script args>...]

set -eu

main="$1"
shift
[[ "${1:-}" == "--" ]] && shift

if [[ "${SYNTRAN_TRANSPILE:-false}" == "true" ]] ; then

	mkdir -p build

	syntran "$main" --transpile build/main.f90

	# -J keeps the .mod files out of the day dir
	#gfortran -O3 -fno-range-check -Wno-tabs -J build -o build/main build/main.f90
	gfortran -fno-range-check -Wno-tabs -J build -o build/main build/main.f90
	#gfortran -fbounds-check -fbacktrace -Wall -Wextra -fno-range-check -Wno-tabs -J build -o build/main build/main.f90

	./build/main "$@"
else
	syntran "$main" -- "$@"
fi


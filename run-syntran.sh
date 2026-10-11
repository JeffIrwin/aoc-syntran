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

# As of 2026-10-11:
# - default profile runs everything in 172 s
# - release (-O3) runs everything in 267 s
# - clearly the longer compile times of O3 do not make up for faster run times.
#   obviously if we linked to a utils/runtime lib instead of recompiling similar
#   code every day this would change. individual days on "difficult" problems
#   might even currently be better with O3

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


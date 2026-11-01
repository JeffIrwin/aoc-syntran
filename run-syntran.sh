#!/usr/bin/env bash

# Run one syntran program with the backend named by SYNTRAN_RUN_BACKEND:
#
#   - vm (default): interpret it with the bytecode VM
#   - transpile:    transpile to Fortran, compile with gfortran, run the binary
#   - native:       compile ahead of time with LLVM (--native), run the binary
#   - jit:          compile with LLVM in memory and run right away (--jit)
#
# native and jit need a syntran built with LLVM.  SYNTRAN overrides which
# syntran executable is used, which is `syntran` from PATH by default.
#
# Usage:  run-syntran.sh main.syntran [-- <script args>...]

set -eu

main="$1"
shift
[[ "${1:-}" == "--" ]] && shift

syntran="${SYNTRAN:-syntran}"

# As of 2026-10-11:
# - default profile runs everything in 172 s
# - release (-O3) runs everything in 267 s
# - clearly the longer compile times of O3 do not make up for faster run times.
#   obviously if we linked to a utils/runtime lib instead of recompiling similar
#   code every day this would change. individual days on "difficult" problems
#   might even currently be better with O3

case "${SYNTRAN_RUN_BACKEND:-vm}" in
vm)
	"$syntran" "$main" -- "$@"
	;;

transpile)
	mkdir -p build

	"$syntran" "$main" --transpile build/main.f90

	# -J keeps the .mod files out of the day dir
	#gfortran -O3 -fno-range-check -Wno-tabs -J build -o build/main build/main.f90
	gfortran -fno-range-check -Wno-tabs -J build -o build/main build/main.f90
	#gfortran -fbounds-check -fbacktrace -Wall -Wextra -fno-range-check -Wno-tabs -J build -o build/main build/main.f90

	./build/main "$@"
	;;

native)
	mkdir -p build

	"$syntran" "$main" --native build/main

	./build/main "$@"
	;;

jit)
	"$syntran" "$main" --jit -- "$@"
	;;

*)
	echo "error: unknown SYNTRAN_RUN_BACKEND \`$SYNTRAN_RUN_BACKEND\`" \
		"(expected vm, transpile, native, or jit)" >&2
	exit 1
	;;
esac


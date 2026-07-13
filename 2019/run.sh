#!/usr/bin/env bash

# Go to every sub dir and run the aoc syntran script

#set -exu
set -eu

echo "starting run.sh"

dirs=$(ls | grep '^[0-9]' | sort -g)

for dir in ${dirs[@]} ; do

	# I started supporting cmd args halfway through solving 2019
	args="-a"

	#[[ "$dir" == "4" ]] && continue
	[[ "$dir" == "18" ]] && args="-a -t -1"

	pushd "$dir" >/dev/null

	main=main.syntran
	grep 'Expect' "$main" | tr -d '\t'

	time syntran "$main" -- $args
	#syntran "$main"

	echo
	popd >/dev/null
done

echo "ending run.sh"


#!/usr/bin/env bash

# Go to every sub dir and run the subdir script

#set -exu
set -eu

echo "starting top-level run.sh"

dirs=$( \
	find . -maxdepth 1 -name "2*" -type d -printf '%P\n' \
	| sort -g\
)

for dir in ${dirs[@]} ; do

	#[[ "$dir" == "4" ]] && continue

	pushd "$dir" >/dev/null

	time ./run.sh

	echo
	popd >/dev/null
done

echo "ending top-level run.sh"


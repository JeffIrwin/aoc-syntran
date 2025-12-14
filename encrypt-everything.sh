#!/usr/bin/env bash

# Go to every sub-sub dir and encrypt the inputs

#set -exu
set -eu

# Don't echo secrets!
set +x

echo "starting top-level encrypt-everything.sh"

year_dirs=$( \
	find . -maxdepth 1 -name "2*" -type d -printf '%P\n' \
	| sort -g\
)

for year_dir in ${year_dirs[@]} ; do

	#[[ "$year_dir" == "2017" ]] && continue

	pushd "$year_dir" >/dev/null

	#time ./run.sh
	day_dirs=$( \
		find . -type d -printf '%P\n' \
		| sort -g\
	)
	
	for day_dir in ${day_dirs[@]} ; do
	
		#[[ "$day_dir" == "4" ]] && continue
	
		echo "$year_dir/$day_dir"
		pushd "$day_dir" >/dev/null
	
		#main=main.syntran
		#grep 'Expect' "$main" | tr -d '\t'
		#time syntran "$main"

		files=("input.txt" "test-input.txt")
		for f in ${files[@]} ; do
			[[ ! -f $f ]] && continue
			input_plaintext="$f"
			gpg --batch --yes --passphrase "$AOC_GPG_PASSPHRASE" -c "$input_plaintext"
			echo Encrypted file $input_plaintext.gpg
			rm "$input_plaintext"
		done
	
		popd >/dev/null
	done

	popd >/dev/null
done

echo "ending top-level encrypt-everything.sh"


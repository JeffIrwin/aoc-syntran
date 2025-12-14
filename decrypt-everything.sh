#!/usr/bin/env bash

# Go to every sub-sub dir and decrypt the inputs

#set -exu
set -eu

# Don't echo secrets!
set +x

echo "starting top-level decrypt-everything.sh"

year_dirs=$( \
	find . -maxdepth 1 -name "2*" -type d -printf '%P\n' \
	| sort -g\
)

for year_dir in ${year_dirs[@]} ; do

	#[[ "$year_dir" == "2017" ]] && continue

	pushd "$year_dir" >/dev/null

	day_dirs=$( \
		find . -type d -printf '%P\n' \
		| sort -g\
	)
	
	for day_dir in ${day_dirs[@]} ; do
	
		#[[ "$day_dir" == "4" ]] && continue
	
		echo "$year_dir/$day_dir"
		pushd "$day_dir" >/dev/null

		files=(*.gpg)
		for f in ${files[@]} ; do
			[[ ! -f $f ]] && continue
			input_ciphertext="$f"
			output_plaintext="${input_ciphertext%.*}"
			gpg --batch --yes --passphrase "$AOC_GPG_PASSPHRASE" -o "$output_plaintext" -d "$input_ciphertext"
			echo Decrypted file $output_plaintext
		done
	
		popd >/dev/null
	done

	popd >/dev/null
done

echo "ending top-level decrypt-everything.sh"


#!/usr/bin/env bash

# Don't echo secrets!
set +x
set -e

if [[ $# -lt 1 ]]; then
	echo "Error: bad command arguments"
	echo "Usage:"
	echo "    decrypt-aoc.sh ciphertext1.txt.gpg [ciphertext2.txt.gpg ...]"
	exit 1
fi

for f in $* ; do
	input_ciphertext="$f"
	output_plaintext="${input_ciphertext%.*}"
	gpg --batch --yes --passphrase "$AOC_GPG_PASSPHRASE" -o "$output_plaintext" -d "$input_ciphertext"
	echo Decrypted file $output_plaintext
	echo
done


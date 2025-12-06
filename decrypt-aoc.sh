#!/usr/bin/env bash

# Don't echo secrets!
set +x
set -e

if [[ $# -ne 1 ]]; then
	echo "Error: bad command arguments"
	echo "Usage:"
	echo "    decrypt-aoc.sh ciphertext.txt.gpg"
	exit 1
fi
input_ciphertext="$1"
output_plaintext="${input_ciphertext%.*}"

gpg --batch --yes --passphrase "$AOC_GPG_PASSPHRASE" -o "$output_plaintext" -d "$input_ciphertext"


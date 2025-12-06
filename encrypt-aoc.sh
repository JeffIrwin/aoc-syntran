#!/usr/bin/env bash

# Don't echo secrets!
set +x
set -e

if [[ $# -ne 1 ]]; then
	echo "Error: bad command arguments"
	echo "Usage:"
	echo "    encrypt-aoc.sh plaintext.txt"
	exit 1
fi
input_plaintext="$1"

gpg --batch --yes --passphrase "$AOC_GPG_PASSPHRASE" -c "$input_plaintext"
rm "$input_plaintext"


#!/usr/bin/env bash

# Don't echo secrets!
set +x
set -e

if [[ $# -lt 1 ]]; then
	echo "Error: bad command arguments"
	echo "Usage:"
	echo "    encrypt-aoc.sh plaintext1.txt [plaintext2.txt ...]"
	exit 1
fi

for f in $* ; do
	input_plaintext="$f"
	gpg --batch --yes --passphrase "$AOC_GPG_PASSPHRASE" -c "$input_plaintext"
	echo Encrypted file $input_plaintext.gpg
	rm "$input_plaintext"
done


#!/bin/bash

#set -xe
set -e

green="\e[92;1m"
magenta="\e[95m"
yellow="\e[93m"
red="\e[91;1m"
bold="\e[;1m"
reset="\e[0m"

# Provide year and date as single arg, e.g. "2023/1" or "2023/13"
#
# For days 1 through 9, make sure to use a single digit.  If you try "01" you
# will get a 404 error
#
# TODO: arg count checking

if [[ "$#" -ne "1" ]]; then
	echo -e "${red}Error${reset}: missing year/day cmd arg"
	echo "Usage:"
	echo "    newday.sh 2024/1"
	exit -1
fi

year=${1%%/*}
day=${1#*/}
echo "year = $year"
echo "day = $day"

# Ban leading 0's because it will 404.  Could automatically remove
if [[ "${day:0:1}" == "0" ]]; then
	echo -e "${red}Error${reset}: cannot use leading 0's in day"
	exit -1
fi

mkdir -p $1
cp template.syntran $1/main.syntran

pushd $1

# The cookies text file can be downloaded via firefox addin "cookies.txt"
#
# To save cookies, click the puzzle-shaped icon in the top-right of Firefox to
# access your extensions.  Then click on cookies.txt and "open extension" and
# finally "current site" (or "ALL").  Take whichever file gets exported for AOC
# and copy it to "~/cookies.txt", which is where the curl command below expects
# to find your cookies
#
# There are built-in browser features that can probably do this more easily
#
curl --cookie ~/cookies.txt https://adventofcode.com/${year}/day/${day}/input -o input.txt

popd

echo -e "${green}Done!${reset}"



# aoc-syntran

[Advent of code](https://adventofcode.com/) in [syntran](https://adventofcode.com/)

## What is syntran?

Syntran is my interpretted, array-oriented language.  The interpreter is written in this repo:  https://github.com/JeffIrwin/syntran

⚠️ Syntran is pre-alpha and I don't recommend using it for anything serious.  You will discover bugs, missing features, many pain points in general; and later updates will be incompatible

See the [README](https://github.com/JeffIrwin/syntran) in the syntran repo for instructions on how to build the interpretter.  You will need a Fortran compiler 

After building, copy the `syntran` interpreter to a directory in your `$PATH` environment variable for convenience 

## Running advent of code

To run a solution for AOC, change to the directory for that day's solution and run the syntran interpretter.  For example:

```
cd 2023/01
syntran ./main.syntran
```

As of 2023-12-25, you will need at least syntran version 0.0.34 to run the code here

<!-- As of 2023-12-15, you will need at least syntran version 0.0.30 to run the code here -->

Until 2023 day 19, 0.0.33 was sufficient.  After that, the `exit()` fn and mixed `i64`/`f32` operations are required

Until 2023 day 15, 0.0.29 was sufficient.  Day 15 requires `i32()` casting characters to ASCII codes, which were added in 0.0.30

Until 2023 day 5, 0.0.27 was sufficient.  Day 5 requires `i32()` casting and `parse_i64()`, which were added by 0.0.29

There are more details about which version you need for which day that I'm too lazy to document here, but just use the latest and greatest.  For details, check the commits in the syntran repo

## Development streams

I'm developing these solutions live on twitch.  You can also watch them after the fact at this YouTube playlist:  https://www.youtube.com/watch?v=xrWKYiD6Xr4&list=PLkNcKcm8wEj4TYgBqtaCBqKkfJsXOv49E


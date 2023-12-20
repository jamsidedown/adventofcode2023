# Advent of Code 2023

OCaml solutions to as much of the 2023 Advent of Code as I can solve.

## Setup

This year's advent of code will be attempted in OCaml.

Rather than each day having its own directory with an executable script, I'm going to attempt to split my solution into `bin`, `src`, and `test` directories.

I'm using Visual Studio Code as my IDE with the `OCaml Platform` plugin.

When I'm stuck I've fallen back on C#.

### Software versions

```sh
$ ocaml --version
The OCaml toplevel, version 5.1.0

$ dune --version
3.11.1

$ opam --version
2.1.5

$ dotnet --version
8.0.100
```

## Building

```sh
$ dune build
```

## Running unit tests

```sh
$ dune test
```

## Running solutions

> I've not included the input files this year, as I think there may some copyright issues around that.
>
> To run the solutions you'll need to download and save your input files into the `input` directory.
>
> Naming convention is `day01.txt`, `day02.txt`, etc.

### OCaml

```sh
# to run most recent day
$ dune exec aoc23

# to run a specific day(s)
$ dune exec aoc23 1 2 3

# to run all days
$ dune exec aoc23 all
```

### C#

For the days where I've got stuck with OCaml, each individual day's dotnet project can be built and run.

```sh
cd dotnet/DayTwelve
dotnet restore
dotnet build
dotnet run
```
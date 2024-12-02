# 🎄 Advent of Clerk 2024

[Advent of Code 2024](https://adventofcode.com) with [Clerk](https://clerk.vision).

## Usage

To view the notebooks, run:

``` shell
clj -M:nextjournal/clerk nextjournal.clerk/serve! --watch-paths src --port 7878 --browse
```

This will start the Clerk webserver on port 7878 and watch the `src/`
directory for changes and open Clerk in your browser.

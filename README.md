### Desc

Haskell implementation of Maelstrom sandbox 

# Hastrom

## About
Playing with [maelstrom](https://github.com/jepsen-io/maelstrom) (A workbench for writing toy implementations of distributed systems) in haskell.

## Quick Start 

### Requirement
You need cabal, GHC and [maelstrom](https://github.com/jepsen-io/maelstrom).
The easiest way to get the first two is using HLS.


Verified compatibility with:
- HLS - 1.10.0.0
- cabal - 3.10.1.0
- GHC - 9.2.7 (base-4.16.4.0)
- maelstrom - 0.2.3

### Usage
```bash
cabal build
```
Sample path to build executable: `./dist-newstyle/build/x86_64-linux/ghc-9.2.7/hastrom-0.1.0.0/x/hastrom/build/hastrom/hastrom`
```bash
./path/to/maelstrom test -w broadcast --bin ./path/to/exe --nodes n1 --time-limit 20 --log-stderr
```

## Contributions

If you would like to contribute feel free to create an issue, fork and create a PR.

## Acknowledgments
Special thanks to:
- @jepsen-io for creating workbench to easier create distributed app [maelstrom](https://github.com/jepsen-io/maelstrom)
- @jonhoo for creating [video](https://www.youtube.com/watch?v=gboGyccRVXI) about maelstrom 
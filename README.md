Playing with [maelstrom](https://github.com/jepsen-io/maelstrom) in haskell.

## Needed
HLS - 1.10.0.0
cabal - 3.10.1.0
GHC - 9.2.7 (base-4.16.4.0)
maelstrom - 0.2.3

## Start
```bash
cabal build
// sample path to build exe: ./dist-newstyle/build/x86_64-linux/ghc-9.2.7/hastrom-0.1.0.0/x/hastrom/build/hastrom/hastrom
./path/to/maelstrom test -w broadcast --bin ./path/to/exe --nodes n1 --time-limit 20 --log-stderr
```

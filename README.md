# stratagem
A Scheme-like programming language implemented in Haskell

### Developing

You need Haskell, and a build tool such as Cabal or Stack.
I like being modern, so I'm using Stack.
I recommend using the new kid around the block, [GHCup](https://haskell.org/ghcup), to install all of these.

To build:

``` shell
stack build --fast --file-watch
```

To boot the REPL:

``` shell
stack build
stack exec stratagem -- -r
```

To run the executable:

``` shell
stack build
stack exec stratagem -- "$@"
```


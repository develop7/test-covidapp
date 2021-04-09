# test-covidapp

## Build

```
$ stack build
```

It doesn't depend on `stack` though, the `cabal` should be able to build it as well; just make sure the GHC is 8.10 (because of the `base` version bounds) and project is built with `new-build`

## Run

```
$ stack exec test-covidapp-exe
```

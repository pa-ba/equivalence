[![Hackage](https://img.shields.io/hackage/v/equivalence.svg?color=informational)](https://hackage.haskell.org/package/equivalence)
[![equivalence on Stackage Nightly](https://stackage.org/package/equivalence/badge/nightly)](https://stackage.org/nightly/package/equivalence)
[![Stackage LTS version](https://www.stackage.org/package/equivalence/badge/lts?label=Stackage)](https://www.stackage.org/package/equivalence)
[![Haskell CI](https://github.com/pa-ba/equivalence/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/pa-ba/equivalence/actions/workflows/haskell-ci.yml)

# equivalence

This is an implementation of Tarjan's Union-Find algorithm (Robert
E. Tarjan. "Efficiency of a Good But Not Linear Set Union
Algorithm", JACM 22(2), 1975) in order to maintain an equivalence
relation.

This implementation is a port of the `union-find` package using the
`ST` monad transformer (instead of the `IO` monad).

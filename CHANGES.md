0.4.1.1
-------

_Andreas Abel, 2025-08-27_

- bump cabal-version to 1.18
- remove unused dependencies

Tested with GHC 8.0 - 9.14 alpha1

0.4.1
-----

_Andreas Abel, 2022-07-26_

* New methods `values` and `classes` to get all values and classes encountered
  [#7](https://github.com/pa-ba/equivalence/issues/7)
  [#13](https://github.com/pa-ba/equivalence/pull/13)
  (contributed by Jimmy Koppel).

Tested with GHC 7.10 - 9.4.1 RC1.

0.4.0.1
-------

_Andreas Abel, 2022-05-26_

* add `LANGUAGE TypeOperators` for GHC 9.4

0.4
---

_Andreas Abel, 2022-02-03_

* remove `ErrorT` instance for compatibility with `transformers-0.6` and `mtl-2.3`

0.3.5
-----

_Patrick Bahr, 2019-09-09_

* compatibility with GHC 8.8

0.3.4
-----
* `MonadFail` instance for `EquivT`

0.3.3
-----
* compatibility with GHC 8.6

0.3.2
-----
* add `Applicative` constraints for backwards compatibility with GHC 7.8

0.3.1
-----
* use `transformers-compat` for backwards compatibility with older versions of `transformers`

0.3.0.1
-------
* add `CHANGES.txt` to `.cabal` file

0.3
---
* add suport for `Control.Monad.Except` (thus the new dependency constraint `mtl >= 2.2.1`)

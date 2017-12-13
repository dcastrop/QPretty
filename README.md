# QPretty

This package offers a wrapper on top of the [prettyprinter][1] package for
defining prettyprinting functions. It re-exports module
[Data.Text.Prettyprint.Doc][1], and provides an abstract datatype, `EDoc`,
which is built using the `ppr` Quasiquoter.

## EDoc

Existential type that contains elements of class  [`Pretty`][1].

## EDoc/Q

Quasiquoter for writing [EDoc](#edoc) expressions conveniently. Any arbitrary
_Haskell_ expression is wrapped in type `EDoc`.


[1]: https://hackage.haskell.org/package/prettyprinter

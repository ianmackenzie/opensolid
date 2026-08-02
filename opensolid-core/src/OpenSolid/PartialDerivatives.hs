module OpenSolid.PartialDerivatives (merge) where

import OpenSolid.Pair qualified as Pair

{-| Implement structural sharing of surface partial derivatives.

For a given surface function `f` (function of two parameters),
its partial derivatives with respect to parameters U and V look like:

        f
      /   \
     fu   fv
    /  \ /  \
  fuu  fuv  fvv

That is, the partial derivative of df/du with respect to V
will always be equal to the partial derivative of df/dv with respect to U.

This function takes a given `(fu, fv)` pair and returns a new "merged" pair `(fu, fv')`,
where `fv'` is a copy of `fv` that shares the `fuv` second derivative with `fu`.

The arguments are:

- a function to construct a new (derivative) function
  from its compiled form and its own two partial derivatives
- a function to get the compiled form of a (derivative) function,
  so that the compiled form can be copied across from `fv` to `fv'`
- a function to get the two partial derivative of a given (derivative) function
- the initial two partial derivatives '(fu, fv)' to merge
-}
merge ::
  (compiledDerivative -> (derivative, derivative) -> derivative) ->
  (derivative -> compiledDerivative) ->
  (derivative -> (derivative, derivative)) ->
  (derivative, derivative) ->
  (derivative, derivative)
merge new compiled partialDerivatives (fu, fv) = do
  let fuv = Pair.second (partialDerivatives fu)
  let fvv = Pair.second (partialDerivatives fv)
  let fv' = new (compiled fv) (fuv, fvv)
  (fu, fv')

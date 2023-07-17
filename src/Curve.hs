module Curve
  ( Approximation
  , approximation
  , estimate
  , refine
  )
where

import Domain (Domain)
import Domain qualified
import OpenSolid
import Range (Range)
import Range qualified

data Approximation units
  = Approximation
      (Range units -> Range units -> Range units)
      (Domain -> Range units)
      (ApproximationNode units)
      (Range units)

data ApproximationNode units
  = ApproximationLeaf Domain
  | ApproximationNode
      (ApproximationNode units)
      (Range units)
      (ApproximationNode units)
      (Range units)

approximation ::
  (Range units -> Range units -> Range units) ->
  (Domain -> Range units) ->
  Approximation units
approximation combinator estimator =
  Approximation combinator estimator (ApproximationLeaf Domain.unit) (estimator Domain.unit)

estimate :: Approximation units -> Range units
estimate (Approximation _ _ _ range) = range

refine :: Approximation units -> Approximation units
refine (Approximation combinator estimator root _) =
  let (updatedRoot, updatedEstimate) = refineNode combinator estimator root
   in Approximation combinator estimator updatedRoot updatedEstimate

refineNode ::
  (Range units -> Range units -> Range units) ->
  (Domain -> Range units) ->
  ApproximationNode units ->
  (ApproximationNode units, Range units)
refineNode combinator estimator (ApproximationLeaf domain) =
  let (leftDomain, rightDomain) = Range.bisect domain
      leftChild = ApproximationLeaf leftDomain
      leftEstimate = estimator leftDomain
      rightChild = ApproximationLeaf rightDomain
      rightEstimate = estimator rightDomain
      refinedNode = ApproximationNode leftChild leftEstimate rightChild rightEstimate
      refinedEstimate = combinator leftEstimate rightEstimate
   in (refinedNode, refinedEstimate)
refineNode combinator estimator (ApproximationNode leftChild leftEstimate rightChild rightEstimate) =
  let leftWidth = Range.width leftEstimate
      rightWidth = Range.width rightEstimate
   in if leftWidth >= rightWidth
        then
          let (refinedLeftChild, refinedLeftEstimate) = refineNode combinator estimator leftChild
           in if rightWidth >= Range.width refinedLeftEstimate
                then
                  let (refinedRightChild, refinedRightEstimate) =
                        refineNode combinator estimator rightChild
                      refinedEstimate = combinator refinedLeftEstimate refinedRightEstimate
                      refinedNode =
                        ApproximationNode
                          refinedLeftChild
                          refinedLeftEstimate
                          refinedRightChild
                          refinedRightEstimate
                   in (refinedNode, refinedEstimate)
                else
                  let (twiceRefinedLeftChild, twiceRefinedLeftEstimate) =
                        refineNode combinator estimator refinedLeftChild
                      refinedEstimate = combinator twiceRefinedLeftEstimate rightEstimate
                      refinedNode =
                        ApproximationNode
                          twiceRefinedLeftChild
                          twiceRefinedLeftEstimate
                          rightChild
                          rightEstimate
                   in (refinedNode, refinedEstimate)
        else
          let (refinedRightChild, refinedRightEstimate) = refineNode combinator estimator rightChild
           in if leftWidth >= Range.width refinedRightEstimate
                then
                  let (refinedLeftChild, refinedLeftEstimate) =
                        refineNode combinator estimator leftChild
                      refinedEstimate = combinator refinedLeftEstimate refinedRightEstimate
                      refinedNode =
                        ApproximationNode
                          refinedLeftChild
                          refinedLeftEstimate
                          refinedRightChild
                          refinedRightEstimate
                   in (refinedNode, refinedEstimate)
                else
                  let (twiceRefinedRightChild, twiceRefinedRightEstimate) =
                        refineNode combinator estimator refinedRightChild
                      refinedEstimate = combinator leftEstimate twiceRefinedRightEstimate
                      refinedNode =
                        ApproximationNode
                          leftChild
                          leftEstimate
                          twiceRefinedRightChild
                          twiceRefinedRightEstimate
                   in (refinedNode, refinedEstimate)

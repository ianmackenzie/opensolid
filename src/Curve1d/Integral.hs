module Curve1d.Integral
  ( Integral
  , ofCurve
  )
where

import {-# SOURCE #-} Curve1d (Curve1d)
import {-# SOURCE #-} Curve1d qualified
import Domain (Domain)
import Domain qualified
import Estimate (Estimate (Estimate), IsEstimate (..))
import OpenSolid
import Qty qualified
import Range (Range)
import Range qualified

data Integral units
  = Integral (Curve1d units) (Curve1d units) (IntegrationNode units) (Range units)

data IntegrationNode units
  = IntegrationLeaf Domain
  | IntegrationNode (IntegrationNode units) (Range units) (IntegrationNode units) (Range units)

ofCurve :: Curve1d units -> Integral units
ofCurve curve =
  let derivative = Curve1d.derivative curve
      initialEstimate = differentialEstimate Domain.unit curve derivative
   in Integral curve derivative (IntegrationLeaf Domain.unit) initialEstimate

instance IsEstimate (Integral units) units where
  boundsImpl (Integral _ _ _ bounds) = bounds

  refineImpl (Integral curve derivative rootNode _) =
    let (updatedRootNode, updatedBounds) = refineNode curve derivative rootNode
     in Estimate (Integral curve derivative updatedRootNode updatedBounds)

differentialEstimate :: Domain -> Curve1d units -> Curve1d units -> Range units
differentialEstimate domain curve derivative =
  let dx = Range.width domain
      derivativeBounds = Curve1d.segmentBounds domain derivative
      estimate0 = dx * Curve1d.segmentBounds domain curve
      y1 = Curve1d.evaluateAt (Range.minValue domain) curve
      y2 = Curve1d.evaluateAt (Range.maxValue domain) curve
      m = Range.width derivativeBounds
      error1 = 0.125 * m * dx * dx
      estimate1 = dx * Qty.midpoint y1 y2 + Range.from -error1 error1
   in case Range.intersection estimate0 estimate1 of
        Just intersection -> intersection
        Nothing -> estimate0 -- Shouldn't happen if bounds are correct

refineNode :: Curve1d units -> Curve1d units -> IntegrationNode units -> (IntegrationNode units, Range units)
refineNode curve derivative (IntegrationLeaf domain) =
  let (leftDomain, rightDomain) = Range.bisect domain
      leftChild = IntegrationLeaf leftDomain
      leftEstimate = differentialEstimate leftDomain curve derivative
      rightChild = IntegrationLeaf rightDomain
      rightEstimate = differentialEstimate rightDomain curve derivative
      refinedNode = IntegrationNode leftChild leftEstimate rightChild rightEstimate
      refinedEstimate = leftEstimate + rightEstimate
   in (refinedNode, refinedEstimate)
refineNode curve derivative (IntegrationNode leftChild leftEstimate rightChild rightEstimate) =
  let leftWidth = Range.width leftEstimate
      rightWidth = Range.width rightEstimate
   in if leftWidth >= rightWidth
        then
          let (refinedLeftChild, refinedLeftEstimate) = refineNode curve derivative leftChild
           in if rightWidth >= Range.width refinedLeftEstimate
                then
                  let (refinedRightChild, refinedRightEstimate) =
                        refineNode curve derivative rightChild
                      refinedEstimate = refinedLeftEstimate + refinedRightEstimate
                      refinedNode =
                        IntegrationNode
                          refinedLeftChild
                          refinedLeftEstimate
                          refinedRightChild
                          refinedRightEstimate
                   in (refinedNode, refinedEstimate)
                else
                  let (twiceRefinedLeftChild, twiceRefinedLeftEstimate) =
                        refineNode curve derivative refinedLeftChild
                      refinedEstimate = twiceRefinedLeftEstimate + rightEstimate
                      refinedNode =
                        IntegrationNode
                          twiceRefinedLeftChild
                          twiceRefinedLeftEstimate
                          rightChild
                          rightEstimate
                   in (refinedNode, refinedEstimate)
        else
          let (refinedRightChild, refinedRightEstimate) = refineNode curve derivative rightChild
           in if leftWidth >= Range.width refinedRightEstimate
                then
                  let (refinedLeftChild, refinedLeftEstimate) =
                        refineNode curve derivative leftChild
                      refinedEstimate = refinedLeftEstimate + refinedRightEstimate
                      refinedNode =
                        IntegrationNode
                          refinedLeftChild
                          refinedLeftEstimate
                          refinedRightChild
                          refinedRightEstimate
                   in (refinedNode, refinedEstimate)
                else
                  let (twiceRefinedRightChild, twiceRefinedRightEstimate) =
                        refineNode curve derivative refinedRightChild
                      refinedEstimate = leftEstimate + twiceRefinedRightEstimate
                      refinedNode =
                        IntegrationNode
                          leftChild
                          leftEstimate
                          twiceRefinedRightChild
                          twiceRefinedRightEstimate
                   in (refinedNode, refinedEstimate)

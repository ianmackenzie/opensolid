module OpenSolid.CompiledFunction
  ( CompiledFunction (..)
  , concrete
  , constant
  , abstract
  , IsAbstract (IsAbstract)
  , expression
  , desingularized
  , evaluate
  , evaluateBounds
  , evaluators
  , map
  , map2
  , map3
  , map4
  , debug
  )
where

import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units

data CompiledFunction inputValue outputValue inputBounds outputBounds where
  Concrete ::
    Expression.Evaluation inputValue outputValue inputBounds outputBounds =>
    Expression inputValue outputValue ->
    CompiledFunction inputValue outputValue inputBounds outputBounds
  Abstract ::
    (inputValue -> outputValue) ->
    (inputBounds -> outputBounds) ->
    CompiledFunction inputValue outputValue inputBounds outputBounds

instance
  ( Units.Coercion (Expression inputValue outputValue1) (Expression inputValue outputValue2)
  , Expression.Evaluation inputValue outputValue1 inputBounds outputBounds1
  , Expression.Evaluation inputValue outputValue2 inputBounds outputBounds2
  , Units.Coercion outputValue1 outputValue2
  , Units.Coercion outputBounds1 outputBounds2
  ) =>
  Units.Coercion
    (CompiledFunction inputValue outputValue1 inputBounds outputBounds1)
    (CompiledFunction inputValue outputValue2 inputBounds outputBounds2)
  where
  coerce (Concrete expr) = Concrete (Units.coerce expr)
  coerce (Abstract value bounds) = Abstract (Units.coerce . value) (Units.coerce . bounds)

instance
  ( Expression.Evaluation inputValue outputValue inputBounds outputBounds
  , Negation (Expression inputValue outputValue)
  , Negation outputValue
  , Negation outputBounds
  ) =>
  Negation (CompiledFunction inputValue outputValue inputBounds outputBounds)
  where
  negative = map negative negative negative

instance
  ( Expression.Evaluation inputValue outputValue inputBounds outputBounds
  , Negation (Expression inputValue outputValue)
  , Negation outputValue
  , Negation outputBounds
  ) =>
  Multiplication
    Sign
    (CompiledFunction inputValue outputValue inputBounds outputBounds)
    (CompiledFunction inputValue outputValue inputBounds outputBounds)
  where
  Positive .*. compiled = compiled
  Negative .*. compiled = negative compiled

instance
  ( Expression.Evaluation inputValue outputValue inputBounds outputBounds
  , Negation (Expression inputValue outputValue)
  , Negation outputValue
  , Negation outputBounds
  ) =>
  Multiplication
    (CompiledFunction inputValue outputValue inputBounds outputBounds)
    Sign
    (CompiledFunction inputValue outputValue inputBounds outputBounds)
  where
  compiled .*. Positive = compiled
  compiled .*. Negative = negative compiled

instance
  ( inputValue1 ~ inputValue2
  , inputBounds1 ~ inputBounds2
  , Expression.Evaluation inputValue1 outputValue1 inputBounds1 outputBounds1
  , Expression.Evaluation inputValue2 outputValue2 inputBounds2 outputBounds2
  , Expression.Evaluation inputValue1 outputValue3 inputBounds1 outputBounds3
  , Addition
      (Expression inputValue1 outputValue1)
      (Expression inputValue2 outputValue2)
      (Expression inputValue1 outputValue3)
  , Addition outputValue1 outputValue2 outputValue3
  , Addition outputBounds1 outputBounds2 outputBounds3
  ) =>
  Addition
    (CompiledFunction inputValue1 outputValue1 inputBounds1 outputBounds1)
    (CompiledFunction inputValue2 outputValue2 inputBounds2 outputBounds2)
    (CompiledFunction inputValue1 outputValue3 inputBounds1 outputBounds3)
  where
  (.+.) = map2 (.+.) (.+.) (.+.)

instance
  ( inputValue1 ~ inputValue2
  , inputBounds1 ~ inputBounds2
  , Expression.Evaluation inputValue1 outputValue1 inputBounds1 outputBounds1
  , Expression.Evaluation inputValue2 outputValue2 inputBounds2 outputBounds2
  , Expression.Evaluation inputValue1 outputValue3 inputBounds1 outputBounds3
  , Subtraction
      (Expression inputValue1 outputValue1)
      (Expression inputValue2 outputValue2)
      (Expression inputValue1 outputValue3)
  , Subtraction outputValue1 outputValue2 outputValue3
  , Subtraction outputBounds1 outputBounds2 outputBounds3
  ) =>
  Subtraction
    (CompiledFunction inputValue1 outputValue1 inputBounds1 outputBounds1)
    (CompiledFunction inputValue2 outputValue2 inputBounds2 outputBounds2)
    (CompiledFunction inputValue1 outputValue3 inputBounds1 outputBounds3)
  where
  (.-.) = map2 (.-.) (.-.) (.-.)

instance
  ( inputValue1 ~ inputValue2
  , inputBounds1 ~ inputBounds2
  , Expression.Evaluation inputValue1 outputValue1 inputBounds1 outputBounds1
  , Expression.Evaluation inputValue2 outputValue2 inputBounds2 outputBounds2
  , Expression.Evaluation inputValue1 outputValue3 inputBounds1 outputBounds3
  , Multiplication_
      (Expression inputValue1 outputValue1)
      (Expression inputValue2 outputValue2)
      (Expression inputValue1 outputValue3)
  , Multiplication_ outputValue1 outputValue2 outputValue3
  , Multiplication_ outputBounds1 outputBounds2 outputBounds3
  ) =>
  Multiplication_
    (CompiledFunction inputValue1 outputValue1 inputBounds1 outputBounds1)
    (CompiledFunction inputValue2 outputValue2 inputBounds2 outputBounds2)
    (CompiledFunction inputValue1 outputValue3 inputBounds1 outputBounds3)
  where
  (?*?) = map2 (?*?) (?*?) (?*?)

instance
  ( inputValue1 ~ inputValue2
  , inputBounds1 ~ inputBounds2
  , Expression.Evaluation inputValue1 outputValue1 inputBounds1 outputBounds1
  , Expression.Evaluation inputValue2 outputValue2 inputBounds2 outputBounds2
  , Expression.Evaluation inputValue1 outputValue3 inputBounds1 outputBounds3
  , Division_
      (Expression inputValue1 outputValue1)
      (Expression inputValue2 outputValue2)
      (Expression inputValue1 outputValue3)
  , Division_ outputValue1 outputValue2 outputValue3
  , Division_ outputBounds1 outputBounds2 outputBounds3
  ) =>
  Division_
    (CompiledFunction inputValue1 outputValue1 inputBounds1 outputBounds1)
    (CompiledFunction inputValue2 outputValue2 inputBounds2 outputBounds2)
    (CompiledFunction inputValue1 outputValue3 inputBounds1 outputBounds3)
  where
  (?/?) = map2 (?/?) (?/?) (?/?)

instance
  ( inputValue1 ~ inputValue2
  , inputBounds1 ~ inputBounds2
  , Expression.Evaluation inputValue1 outputValue1 inputBounds1 outputBounds1
  , Expression.Evaluation inputValue2 outputValue2 inputBounds2 outputBounds2
  , Expression.Evaluation inputValue1 outputValue3 inputBounds1 outputBounds3
  , Multiplication
      (Expression inputValue1 outputValue1)
      (Expression inputValue2 outputValue2)
      (Expression inputValue1 outputValue3)
  , Multiplication outputValue1 outputValue2 outputValue3
  , Multiplication outputBounds1 outputBounds2 outputBounds3
  ) =>
  Multiplication
    (CompiledFunction inputValue1 outputValue1 inputBounds1 outputBounds1)
    (CompiledFunction inputValue2 outputValue2 inputBounds2 outputBounds2)
    (CompiledFunction inputValue1 outputValue3 inputBounds1 outputBounds3)
  where
  (.*.) = map2 (.*.) (.*.) (.*.)

instance
  ( inputValue1 ~ inputValue2
  , inputBounds1 ~ inputBounds2
  , Expression.Evaluation inputValue1 outputValue1 inputBounds1 outputBounds1
  , Expression.Evaluation inputValue2 outputValue2 inputBounds2 outputBounds2
  , Expression.Evaluation inputValue1 outputValue3 inputBounds1 outputBounds3
  , Division
      (Expression inputValue1 outputValue1)
      (Expression inputValue2 outputValue2)
      (Expression inputValue1 outputValue3)
  , Division outputValue1 outputValue2 outputValue3
  , Division outputBounds1 outputBounds2 outputBounds3
  ) =>
  Division
    (CompiledFunction inputValue1 outputValue1 inputBounds1 outputBounds1)
    (CompiledFunction inputValue2 outputValue2 inputBounds2 outputBounds2)
    (CompiledFunction inputValue1 outputValue3 inputBounds1 outputBounds3)
  where
  (./.) = map2 (./.) (./.) (./.)

instance
  ( inputValue1 ~ inputValue2
  , inputBounds1 ~ inputBounds2
  , Expression.Evaluation inputValue1 outputValue1 inputBounds1 outputBounds1
  , Expression.Evaluation inputValue2 outputValue2 inputBounds2 outputBounds2
  , Expression.Evaluation inputValue1 outputValue3 inputBounds1 outputBounds3
  , DotMultiplication_
      (Expression inputValue1 outputValue1)
      (Expression inputValue2 outputValue2)
      (Expression inputValue1 outputValue3)
  , DotMultiplication_ outputValue1 outputValue2 outputValue3
  , DotMultiplication_ outputBounds1 outputBounds2 outputBounds3
  ) =>
  DotMultiplication_
    (CompiledFunction inputValue1 outputValue1 inputBounds1 outputBounds1)
    (CompiledFunction inputValue2 outputValue2 inputBounds2 outputBounds2)
    (CompiledFunction inputValue1 outputValue3 inputBounds1 outputBounds3)
  where
  dot_ = map2 dot_ dot_ dot_

instance
  ( inputValue1 ~ inputValue2
  , inputBounds1 ~ inputBounds2
  , Expression.Evaluation inputValue1 outputValue1 inputBounds1 outputBounds1
  , Expression.Evaluation inputValue2 outputValue2 inputBounds2 outputBounds2
  , Expression.Evaluation inputValue1 outputValue3 inputBounds1 outputBounds3
  , DotMultiplication
      (Expression inputValue1 outputValue1)
      (Expression inputValue2 outputValue2)
      (Expression inputValue1 outputValue3)
  , DotMultiplication outputValue1 outputValue2 outputValue3
  , DotMultiplication outputBounds1 outputBounds2 outputBounds3
  ) =>
  DotMultiplication
    (CompiledFunction inputValue1 outputValue1 inputBounds1 outputBounds1)
    (CompiledFunction inputValue2 outputValue2 inputBounds2 outputBounds2)
    (CompiledFunction inputValue1 outputValue3 inputBounds1 outputBounds3)
  where
  dot = map2 dot dot dot

instance
  ( inputValue1 ~ inputValue2
  , inputBounds1 ~ inputBounds2
  , Expression.Evaluation inputValue1 outputValue1 inputBounds1 outputBounds1
  , Expression.Evaluation inputValue2 outputValue2 inputBounds2 outputBounds2
  , Expression.Evaluation inputValue1 outputValue3 inputBounds1 outputBounds3
  , CrossMultiplication_
      (Expression inputValue1 outputValue1)
      (Expression inputValue2 outputValue2)
      (Expression inputValue1 outputValue3)
  , CrossMultiplication_ outputValue1 outputValue2 outputValue3
  , CrossMultiplication_ outputBounds1 outputBounds2 outputBounds3
  ) =>
  CrossMultiplication_
    (CompiledFunction inputValue1 outputValue1 inputBounds1 outputBounds1)
    (CompiledFunction inputValue2 outputValue2 inputBounds2 outputBounds2)
    (CompiledFunction inputValue1 outputValue3 inputBounds1 outputBounds3)
  where
  cross_ = map2 cross_ cross_ cross_

instance
  ( inputValue1 ~ inputValue2
  , inputBounds1 ~ inputBounds2
  , Expression.Evaluation inputValue1 outputValue1 inputBounds1 outputBounds1
  , Expression.Evaluation inputValue2 outputValue2 inputBounds2 outputBounds2
  , Expression.Evaluation inputValue1 outputValue3 inputBounds1 outputBounds3
  , CrossMultiplication
      (Expression inputValue1 outputValue1)
      (Expression inputValue2 outputValue2)
      (Expression inputValue1 outputValue3)
  , CrossMultiplication outputValue1 outputValue2 outputValue3
  , CrossMultiplication outputBounds1 outputBounds2 outputBounds3
  ) =>
  CrossMultiplication
    (CompiledFunction inputValue1 outputValue1 inputBounds1 outputBounds1)
    (CompiledFunction inputValue2 outputValue2 inputBounds2 outputBounds2)
    (CompiledFunction inputValue1 outputValue3 inputBounds1 outputBounds3)
  where
  cross = map2 cross cross cross

concrete ::
  Expression.Evaluation inputValue outputValue inputBounds outputBounds =>
  Expression inputValue outputValue ->
  CompiledFunction inputValue outputValue inputBounds outputBounds
concrete = Concrete

constant ::
  ( Expression.Constant inputValue outputValue
  , Expression.Evaluation inputValue outputValue inputBounds outputBounds
  ) =>
  outputValue ->
  CompiledFunction inputValue outputValue inputBounds outputBounds
constant value = Concrete (Expression.constant value)

abstract ::
  (inputValue -> outputValue) ->
  (inputBounds -> outputBounds) ->
  CompiledFunction inputValue outputValue inputBounds outputBounds
abstract = Abstract

data IsAbstract = IsAbstract deriving (Eq, Show)

expression ::
  CompiledFunction inputValue outputValue inputBounds outputBounds ->
  Result IsAbstract (Expression inputValue outputValue)
expression (Concrete expr) = Ok expr
expression Abstract{} = Error IsAbstract

desingularized ::
  Expression.Evaluation inputValue outputValue inputBounds outputBounds =>
  CompiledFunction inputValue Number inputBounds (Interval Unitless) ->
  CompiledFunction inputValue outputValue inputBounds outputBounds ->
  CompiledFunction inputValue outputValue inputBounds outputBounds ->
  CompiledFunction inputValue outputValue inputBounds outputBounds ->
  CompiledFunction inputValue outputValue inputBounds outputBounds
desingularized = map4 Expression.desingularized Desingularization.value Desingularization.bounds

map ::
  Expression.Evaluation inputValue outputValue2 inputBounds outputBounds2 =>
  (Expression inputValue outputValue1 -> Expression inputValue outputValue2) ->
  (outputValue1 -> outputValue2) ->
  (outputBounds1 -> outputBounds2) ->
  CompiledFunction inputValue outputValue1 inputBounds outputBounds1 ->
  CompiledFunction inputValue outputValue2 inputBounds outputBounds2
map mapExpression _ _ (Concrete expr) = Concrete (mapExpression expr)
map _ mapValue mapBounds (Abstract value bounds) =
  Abstract (mapValue . value) (mapBounds . bounds)

evaluate ::
  CompiledFunction inputValue outputValue inputBounds outputBounds ->
  inputValue ->
  outputValue
evaluate (Concrete expr) inputValue = Expression.evaluate expr inputValue
evaluate (Abstract value _) inputValue = value inputValue

evaluateBounds ::
  CompiledFunction inputValue outputValue inputBounds outputBounds ->
  inputBounds ->
  outputBounds
evaluateBounds (Concrete expr) inputValue = Expression.evaluateBounds expr inputValue
evaluateBounds (Abstract _ bounds) inputValue = bounds inputValue

evaluators ::
  CompiledFunction inputValue outputValue inputBounds outputBounds ->
  (inputValue -> outputValue, inputBounds -> outputBounds)
evaluators (Concrete expr) = (Expression.evaluate expr, Expression.evaluateBounds expr)
evaluators (Abstract value bounds) = (value, bounds)

map2 ::
  Expression.Evaluation inputValue outputValue3 inputBounds outputBounds3 =>
  ( Expression inputValue outputValue1 ->
    Expression inputValue outputValue2 ->
    Expression inputValue outputValue3
  ) ->
  (outputValue1 -> outputValue2 -> outputValue3) ->
  (outputBounds1 -> outputBounds2 -> outputBounds3) ->
  CompiledFunction inputValue outputValue1 inputBounds outputBounds1 ->
  CompiledFunction inputValue outputValue2 inputBounds outputBounds2 ->
  CompiledFunction inputValue outputValue3 inputBounds outputBounds3
map2 combineExpressions _ _ (Concrete expression1) (Concrete expression2) =
  Concrete (combineExpressions expression1 expression2)
map2 _ combineValues combineBounds compiled1 compiled2 = do
  let (value1, bounds1) = evaluators compiled1
  let (value2, bounds2) = evaluators compiled2
  Abstract
    (\t -> combineValues (value1 t) (value2 t))
    (\t -> combineBounds (bounds1 t) (bounds2 t))

map3 ::
  Expression.Evaluation inputValue outputValue4 inputBounds outputBounds4 =>
  ( Expression inputValue outputValue1 ->
    Expression inputValue outputValue2 ->
    Expression inputValue outputValue3 ->
    Expression inputValue outputValue4
  ) ->
  (outputValue1 -> outputValue2 -> outputValue3 -> outputValue4) ->
  (outputBounds1 -> outputBounds2 -> outputBounds3 -> outputBounds4) ->
  CompiledFunction inputValue outputValue1 inputBounds outputBounds1 ->
  CompiledFunction inputValue outputValue2 inputBounds outputBounds2 ->
  CompiledFunction inputValue outputValue3 inputBounds outputBounds3 ->
  CompiledFunction inputValue outputValue4 inputBounds outputBounds4
map3 combineExpressions _ _ (Concrete expression1) (Concrete expression2) (Concrete expression3) =
  Concrete (combineExpressions expression1 expression2 expression3)
map3 _ combineValues combineBounds compiled1 compiled2 compiled3 = do
  let (value1, bounds1) = evaluators compiled1
  let (value2, bounds2) = evaluators compiled2
  let (value3, bounds3) = evaluators compiled3
  Abstract
    (\t -> combineValues (value1 t) (value2 t) (value3 t))
    (\t -> combineBounds (bounds1 t) (bounds2 t) (bounds3 t))

map4 ::
  Expression.Evaluation inputValue outputValue5 inputBounds outputBounds5 =>
  ( Expression inputValue outputValue1 ->
    Expression inputValue outputValue2 ->
    Expression inputValue outputValue3 ->
    Expression inputValue outputValue4 ->
    Expression inputValue outputValue5
  ) ->
  (outputValue1 -> outputValue2 -> outputValue3 -> outputValue4 -> outputValue5) ->
  (outputBounds1 -> outputBounds2 -> outputBounds3 -> outputBounds4 -> outputBounds5) ->
  CompiledFunction inputValue outputValue1 inputBounds outputBounds1 ->
  CompiledFunction inputValue outputValue2 inputBounds outputBounds2 ->
  CompiledFunction inputValue outputValue3 inputBounds outputBounds3 ->
  CompiledFunction inputValue outputValue4 inputBounds outputBounds4 ->
  CompiledFunction inputValue outputValue5 inputBounds outputBounds5
map4 combineExpressions combineValues combineBounds compiled1 compiled2 compiled3 compiled4
  | Concrete expression1 <- compiled1
  , Concrete expression2 <- compiled2
  , Concrete expression3 <- compiled3
  , Concrete expression4 <- compiled4 =
      Concrete (combineExpressions expression1 expression2 expression3 expression4)
  | otherwise = do
      let (value1, bounds1) = evaluators compiled1
      let (value2, bounds2) = evaluators compiled2
      let (value3, bounds3) = evaluators compiled3
      let (value4, bounds4) = evaluators compiled4
      Abstract
        (\t -> combineValues (value1 t) (value2 t) (value3 t) (value4 t))
        (\t -> combineBounds (bounds1 t) (bounds2 t) (bounds3 t) (bounds4 t))

instance
  ( innerOutputValue ~ outerInputValue
  , innerOutputBounds ~ outerInputBounds
  , Composition
      (Expression innerInputValue innerOutputValue)
      (Expression outerInputValue outerOutputValue)
      (Expression innerInputValue outerOutputValue)
  , Expression.Evaluation innerInputValue outerOutputValue innerInputBounds outerOutputBounds
  ) =>
  Composition
    (CompiledFunction innerInputValue innerOutputValue innerInputBounds innerOutputBounds)
    (CompiledFunction outerInputValue outerOutputValue outerInputBounds outerOutputBounds)
    (CompiledFunction innerInputValue outerOutputValue innerInputBounds outerOutputBounds)
  where
  Concrete outer `compose` Concrete inner = Concrete (outer `compose` inner)
  outer `compose` inner = do
    let (outerValue, outerBounds) = evaluators outer
    let (innerValue, innerBounds) = evaluators inner
    Abstract (outerValue . innerValue) (outerBounds . innerBounds)

debug :: CompiledFunction input output inputBounds outputBounds -> Text
debug (Concrete expr) = Expression.debug expr
debug Abstract{} = "Abstract"

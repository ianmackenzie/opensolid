module Jit
  ( UnaryOp (..)
  , BinaryOp (..)
  , Ast
  , input
  , constant
  , call
  , unary
  , binary
  , compile
  )
where

import Control.Monad.ST (ST, runST)
import Data.Array.MArray qualified as MArray
import Data.Array.ST (STArray)
import Debug (Debug)
import Debug qualified
import GHC.Exts (Any)
import List qualified
import OpenSolid
import Text qualified
import Typeable qualified
import Unsafe.Coerce (unsafeCoerce)
import Prelude qualified

class
  (Known op, Show op, Known input, Show input, Known output) =>
  UnaryOp op input output
    | op -> input
    , op -> output
  where
  evalUnary :: op -> input -> output

class
  (Known op, Show op, Known lhs, Show lhs, Known rhs, Show rhs, Known output) =>
  BinaryOp op lhs rhs output
    | op -> lhs
    , op -> rhs
    , op -> output
  where
  evalBinary :: op -> lhs -> rhs -> output

data Ast input output where
  Constant :: value -> Ast input value
  NonConstant :: NonConstant input output -> Ast input output
  deriving (Eq, Show)

data NonConstant input output where
  Input :: NonConstant input input
  NonInput :: NonInput input output -> NonConstant input output

deriving instance Show (NonConstant input output)

instance (Known input, Known output) => Eq (NonConstant input output) where
  nonConstant1 == nonConstant2 = case nonConstant1 of
    Input | Input <- nonConstant2 -> True | otherwise -> False
    NonInput x | NonInput y <- nonConstant2 -> x == y | otherwise -> False

data NonInput input output where
  Unary ::
    UnaryOp op arg output =>
    op ->
    NonConstant input arg ->
    NonInput input output
  Binary ::
    BinaryOp op lhs rhs output =>
    op ->
    NonConstant input lhs ->
    NonConstant input rhs ->
    NonInput input output

deriving instance Show (NonInput input output)

instance (Known input, Known output) => Eq (NonInput input output) where
  nonInput1 == nonInput2 = case nonInput1 of
    Unary op1 x1 | Unary op2 x2 <- nonInput2 -> Typeable.equal op1 op2 && Typeable.equal x1 x2 | otherwise -> False
    Binary op1 x1 y1 | Binary op2 x2 y2 <- nonInput2 -> Typeable.equal op1 op2 && Typeable.equal x1 x2 && Typeable.equal y1 y2 | otherwise -> False

input :: Ast input input
input = NonConstant Input

constant :: Eq value => value -> Ast input value
constant = Constant

call :: UnaryOp op input output => op -> Ast input output
call op = unary op input

unary :: UnaryOp op arg output => op -> Ast input arg -> Ast input output
unary op (Constant arg) = Constant (evalUnary op arg)
unary op (NonConstant arg) = NonConstant (NonInput (Unary op arg))

binary :: BinaryOp op lhs rhs output => op -> Ast input lhs -> Ast input rhs -> Ast input output
binary op (Constant lhs) (Constant rhs) = Constant (evalBinary op lhs rhs)
binary op (Constant lhs) (NonConstant rhs) = NonConstant (NonInput (Unary (ConstantLhs op lhs) rhs))
binary op (NonConstant lhs) (Constant rhs) = NonConstant (NonInput (Unary (ConstantRhs op rhs) lhs))
binary op (NonConstant lhs) (NonConstant rhs) = NonConstant (NonInput (Binary op lhs rhs))

data ConstantLhs rhs output where
  ConstantLhs :: BinaryOp op lhs rhs output => op -> lhs -> ConstantLhs rhs output

deriving instance Show (ConstantLhs rhs output)

instance Eq (ConstantLhs rhs output) where
  ConstantLhs op1 lhs1 == ConstantLhs op2 lhs2 =
    Typeable.equal op1 op2 && Typeable.equal lhs1 lhs2

instance
  (Known rhs, Show rhs, Known output) =>
  UnaryOp (ConstantLhs rhs output) rhs output
  where
  evalUnary (ConstantLhs binaryOp lhs) rhs = evalBinary binaryOp lhs rhs

data ConstantRhs lhs output where
  ConstantRhs :: BinaryOp op lhs rhs output => op -> rhs -> ConstantRhs lhs output

deriving instance Show (ConstantRhs lhs output)

instance Eq (ConstantRhs lhs output) where
  ConstantRhs op1 rhs1 == ConstantRhs op2 rhs2 =
    Typeable.equal op1 op2 && Typeable.equal rhs1 rhs2

instance
  (Known lhs, Show lhs, Known output) =>
  UnaryOp (ConstantRhs lhs output) lhs output
  where
  evalUnary (ConstantRhs binaryOp rhs) lhs = evalBinary binaryOp lhs rhs

instance
  (inner1 ~ inner2, Known output) =>
  Composition (Ast input inner1) (Ast inner2 output) (Ast input output)
  where
  Constant value . _ = Constant value
  outer . Constant value = Constant (compile outer $ value)
  NonConstant x . NonConstant y = NonConstant (x . y)

instance
  (inner1 ~ inner2, Known output) =>
  Composition (NonConstant input inner1) (NonConstant inner2 output) (NonConstant input output)
  where
  Input . inner = inner
  outer . Input = outer
  NonInput outer . inner = NonInput (outer . inner)

instance
  (inner1 ~ inner2, Known output) =>
  Composition (NonConstant input inner1) (NonInput inner2 output) (NonInput input output)
  where
  Unary op arg . inner = Unary op (arg . inner)
  Binary op arg1 arg2 . inner = Binary op (arg1 . inner) (arg2 . inner)

data Evaluation input where
  Evaluation :: NonInput input output -> Evaluation input

deriving instance Show (Evaluation input)

instance Eq (Evaluation input) where
  Evaluation nonInput1 == Evaluation nonInput2 = equalNonInputs nonInput1 nonInput2

newtype Code input = Code (forall state. input -> STArray state Int Any -> ST state ())

data Computation input = Computation (List (Evaluation input)) (Code input)

evalNonInput :: NonInput input output -> Computation input -> (Computation input, Int)
evalNonInput nonInput computation = do
  let Computation evaluations0 (Code runEvaluations0) = computation
  case List.indexOf (Evaluation nonInput) evaluations0 of
    Just index -> (computation, index)
    Nothing -> case nonInput of
      Unary op Input -> do
        let outputIndex = List.length evaluations0
        let updatedEvaluations = evaluations0 + [Evaluation nonInput]
        let updatedCode = Code \inputValue locals -> Prelude.do
              runEvaluations0 inputValue locals
              let output = unsafeCoerce (evalUnary op (unsafeCoerce inputValue))
              MArray.writeArray locals outputIndex output
        (Computation updatedEvaluations updatedCode, outputIndex)
      Unary op (NonInput arg) -> do
        let (argComputation, argIndex) = evalNonInput arg computation
        let Computation argEvaluations (Code runArgEvaluations) = argComputation
        let outputIndex = List.length argEvaluations
        let updatedEvaluations = argEvaluations + [Evaluation nonInput]
        let updatedCode = Code \inputValue locals -> Prelude.do
              runArgEvaluations inputValue locals
              argValue <- MArray.readArray locals argIndex
              let output = unsafeCoerce (evalUnary op (unsafeCoerce argValue))
              MArray.writeArray locals outputIndex output
        (Computation updatedEvaluations updatedCode, outputIndex)
      Binary op Input Input -> do
        let outputIndex = List.length evaluations0
        let updatedEvaluations = evaluations0 + [Evaluation nonInput]
        let updatedCode = Code \inputValue locals -> Prelude.do
              runEvaluations0 inputValue locals
              let output = unsafeCoerce (evalBinary op (unsafeCoerce inputValue) (unsafeCoerce inputValue))
              MArray.writeArray locals outputIndex output
        (Computation updatedEvaluations updatedCode, outputIndex)
      Binary op Input (NonInput rhs) -> do
        let (rhsComputation, rhsIndex) = evalNonInput rhs computation
        let Computation rhsEvaluations (Code runRhsEvaluations) = rhsComputation
        let outputIndex = List.length rhsEvaluations
        let updatedEvaluations = rhsEvaluations + [Evaluation nonInput]
        let updatedCode = Code \inputValue locals -> Prelude.do
              runRhsEvaluations inputValue locals
              rhsValue <- MArray.readArray locals rhsIndex
              let output = unsafeCoerce (evalBinary op (unsafeCoerce inputValue) (unsafeCoerce rhsValue))
              MArray.writeArray locals outputIndex output
        (Computation updatedEvaluations updatedCode, outputIndex)
      Binary op (NonInput lhs) Input -> do
        let (lhsComputation, lhsIndex) = evalNonInput lhs computation
        let Computation lhsEvaluations (Code runLhsEvaluations) = lhsComputation
        let outputIndex = List.length lhsEvaluations
        let updatedEvaluations = lhsEvaluations + [Evaluation nonInput]
        let updatedCode = Code \inputValue locals -> Prelude.do
              runLhsEvaluations inputValue locals
              lhsValue <- MArray.readArray locals lhsIndex
              let output = unsafeCoerce (evalBinary op (unsafeCoerce lhsValue) (unsafeCoerce inputValue))
              MArray.writeArray locals outputIndex output
        (Computation updatedEvaluations updatedCode, outputIndex)
      Binary op (NonInput lhs) (NonInput rhs) -> do
        let (lhsComputation, lhsIndex) = evalNonInput lhs computation
        let (rhsComputation, rhsIndex) = evalNonInput rhs lhsComputation
        let Computation argEvaluations (Code runArgEvaluations) = rhsComputation
        let outputIndex = List.length argEvaluations
        let updatedEvaluations = argEvaluations + [Evaluation nonInput]
        let updatedCode = Code \inputValue locals -> Prelude.do
              runArgEvaluations inputValue locals
              lhsValue <- MArray.readArray locals lhsIndex
              rhsValue <- MArray.readArray locals rhsIndex
              let output = unsafeCoerce (evalBinary op (unsafeCoerce lhsValue) (unsafeCoerce rhsValue))
              MArray.writeArray locals outputIndex output
        (Computation updatedEvaluations updatedCode, outputIndex)

compile :: Ast input output -> (input -> output)
compile ast = case ast of
  Constant value -> always value
  NonConstant nonConstant -> compileNonConstant nonConstant

compileNonConstant :: NonConstant input output -> (input -> output)
compileNonConstant nonConstant = case nonConstant of
  Input -> identity
  NonInput nonInput -> compileNonInput nonInput

compileNonInput :: NonInput input output -> (input -> output)
compileNonInput nonInput = case nonInput of
  Unary op Input -> evalUnary op
  Unary op (NonInput arg) -> evalUnary op . compileNonInput arg
  Binary op Input Input -> \inputValue -> evalBinary op inputValue inputValue
  Binary op Input (NonInput rhs) -> do
    let evalRhs = compileNonInput rhs
    \inputValue -> evalBinary op inputValue (evalRhs inputValue)
  Binary op (NonInput lhs) Input -> do
    let evalLhs = compileNonInput lhs
    \inputValue -> evalBinary op (evalLhs inputValue) inputValue
  Binary op (NonInput lhs) (NonInput rhs) -> do
    let noOp = Code \_ _ -> Prelude.return ()
    let computation0 = Computation [] noOp
    let (computation1, lhsIndex) = evalNonInput lhs computation0
    let (computation2, rhsIndex) = evalNonInput rhs computation1
    let Computation evaluations (Code runEvaluations) = computation2
    let numEvaluations = List.length evaluations
    \inputValue -> runST Prelude.do
      locals <- MArray.newArray_ (0, numEvaluations - 1)
      runEvaluations inputValue locals
      lhsValue <- MArray.readArray locals lhsIndex
      rhsValue <- MArray.readArray locals rhsIndex
      Prelude.return (evalBinary op (unsafeCoerce lhsValue) (unsafeCoerce rhsValue))

_debugEvaluations :: List (Evaluation input) -> Debug
_debugEvaluations [] = Debug.print "No evaluations"
_debugEvaluations (NonEmpty evaluations) = do
  Debug.print "Evaluations:"
  printEvaluations evaluations

printEvaluations :: NonEmpty (Evaluation input) -> Debug
printEvaluations evaluations = case evaluations of
  first :| [] -> printEvaluation first
  first :| NonEmpty rest -> do
    printEvaluation first
    printEvaluations rest

printEvaluation :: Evaluation input -> Debug
printEvaluation (Evaluation (Unary op _)) = Debug.print ("  " + Text.show op)
printEvaluation (Evaluation (Binary op _ _)) = Debug.print ("  " + Text.show op)

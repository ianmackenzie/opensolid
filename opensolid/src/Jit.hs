module Jit
  ( Value
  , Expr (..)
  , UnaryOp (..)
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
import Typeable (Typeable)
import Typeable qualified
import Unsafe.Coerce (unsafeCoerce)
import Prelude qualified

type Value value = (Eq value, Typeable value)

class
  Value expr =>
  Expr expr input output
    | expr -> input
    , expr -> output
  where
  toAst :: expr -> Ast input output

class
  (Value op, Show op, Value input, Show input, Value output) =>
  UnaryOp op input output
    | op -> input
    , op -> output
  where
  evalUnary :: op -> input -> output

class
  (Value op, Show op, Value lhs, Show lhs, Value rhs, Show rhs, Value output) =>
  BinaryOp op lhs rhs output
    | op -> lhs
    , op -> rhs
    , op -> output
  where
  evalBinary :: op -> lhs -> rhs -> output

data Ast input output where
  Constant :: Value value => value -> Ast input value
  NonConstant :: NonConstant input output -> Ast input output

instance Eq (Ast input output) where
  Constant x == Constant y = Typeable.equal x y
  NonConstant x == NonConstant y = x == y
  Constant{} == NonConstant{} = False
  NonConstant{} == Constant{} = False

data NonConstant input output where
  Input :: NonConstant input input
  NonInput :: NonInput input output -> NonConstant input output

deriving instance Show (NonConstant input output)

instance Eq (NonConstant input output) where (==) = equalNonConstants

equalNonConstants :: NonConstant input a -> NonConstant input b -> Bool
equalNonConstants Input Input = True
equalNonConstants (NonInput x) (NonInput y) = equalNonInputs x y
equalNonConstants Input NonInput{} = False
equalNonConstants NonInput{} Input = False

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

equalNonInputs :: NonInput input a -> NonInput input b -> Bool
equalNonInputs (Unary op1 x1) (Unary op2 x2) =
  Typeable.equal op1 op2 && equalNonConstants x1 x2
equalNonInputs (Binary op1 x1 y1) (Binary op2 x2 y2) =
  Typeable.equal op1 op2 && equalNonConstants x1 x2 && equalNonConstants y1 y2
equalNonInputs Unary{} Binary{} = False
equalNonInputs Binary{} Unary{} = False

instance Eq (NonInput input output) where (==) = equalNonInputs

input :: Ast input input
input = NonConstant Input

constant :: Value value => value -> Ast input value
constant = Constant

call :: UnaryOp op input output => op -> Ast input output
call op = NonConstant (NonInput (Unary op Input))

unary :: (UnaryOp op arg output, Expr expr input arg) => op -> expr -> Ast input output
unary op expr = case toAst expr of
  Constant value -> Constant (evalUnary op value)
  NonConstant nonConstant -> NonConstant (NonInput (Unary op nonConstant))

binary ::
  ( BinaryOp op lhs rhs output
  , Expr expr1 input lhs
  , Expr expr2 input rhs
  ) =>
  op ->
  expr1 ->
  expr2 ->
  Ast input output
binary op arg1 arg2 = applyBinary op (toAst arg1) (toAst arg2)

applyBinary :: BinaryOp op lhs rhs output => op -> Ast input lhs -> Ast input rhs -> Ast input output
applyBinary op (Constant lhs) (Constant rhs) = Constant (evalBinary op lhs rhs)
applyBinary op (Constant lhs) (NonConstant rhs) = NonConstant (NonInput (Unary (ConstantLhs op lhs) rhs))
applyBinary op (NonConstant lhs) (Constant rhs) = NonConstant (NonInput (Unary (ConstantRhs op rhs) lhs))
applyBinary op (NonConstant lhs) (NonConstant rhs) = NonConstant (NonInput (Binary op lhs rhs))

data ConstantLhs rhs output where
  ConstantLhs :: BinaryOp op lhs rhs output => op -> lhs -> ConstantLhs rhs output

deriving instance Show (ConstantLhs rhs output)

instance Eq (ConstantLhs rhs output) where
  ConstantLhs op1 lhs1 == ConstantLhs op2 lhs2 =
    Typeable.equal op1 op2 && Typeable.equal lhs1 lhs2

instance
  (Value rhs, Show rhs, Value output) =>
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
  (Value lhs, Show lhs, Value output) =>
  UnaryOp (ConstantRhs lhs output) lhs output
  where
  evalUnary (ConstantRhs binaryOp rhs) lhs = evalBinary binaryOp lhs rhs

instance
  (inner1 ~ inner2, Value output) =>
  Composition (Ast input inner1) (Ast inner2 output) (Ast input output)
  where
  Constant value . _ = Constant value
  outer . Constant value = Constant (compileAst outer value)
  NonConstant x . NonConstant y = NonConstant (x . y)

instance
  (inner1 ~ inner2, Value output) =>
  Composition (NonConstant input inner1) (NonConstant inner2 output) (NonConstant input output)
  where
  Input . inner = inner
  outer . Input = outer
  NonInput x . y = NonInput (x . y)

instance
  (inner1 ~ inner2, Value output) =>
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

compile :: Expr expr input output => expr -> (input -> output)
compile = toAst >> compileAst

compileAst :: Ast input output -> (input -> output)
compileAst ast = case ast of
  Constant value -> always value
  NonConstant nonConstant -> compileNonConstant nonConstant

compileNonConstant :: NonConstant input output -> (input -> output)
compileNonConstant nonConstant = case nonConstant of
  Input -> identity
  NonInput nonInput -> compileNonInput nonInput

compileNonInput :: NonInput input output -> (input -> output)
compileNonInput nonInput = case nonInput of
  Unary op Input -> evalUnary op
  Unary op arg -> evalUnary op . compileNonConstant arg
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

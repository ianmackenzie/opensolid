module PythonAST
  ( string
  , var
  , call
  , plus
  , eq
  , is
  , dot
  , set
  , int
  , cls
  , ident
  , subscript
  , and
  , returnStatement
  , stmtExpr
  , conditional
  , or
  , list
  , def
  , staticmethod
  , literalStatement
  , literalStatements
  , Statement
  , Expr
  , prettyStatements
  )
where

import Control.Monad (void)
import Language.Python.Common qualified as P
import Language.Python.Version3.Parser qualified as Parser
import List qualified
import OpenSolid
import Prelude (Either (Left, Right))

type Statement = P.Statement ()

type Expr = P.Expr ()

type Ident = P.Ident ()

prettyStatements :: List Statement -> String
prettyStatements stmts =
  P.prettyText (P.Module stmts)

returnStatement :: P.Expr () -> P.Statement ()
returnStatement expr =
  P.Return (Just expr) ()

conditional :: List (Expr, List Statement) -> List Statement -> Statement
conditional conditions elseStmt =
  P.Conditional conditions elseStmt ()

-- Expression as a statement
stmtExpr :: Expr -> Statement
stmtExpr expr = P.StmtExpr expr ()

literalStatement :: String -> List Statement
literalStatement line =
  case Parser.parseStmt line "<literal>" of
    Left err -> internalError (show err)
    Right (statements, _) -> List.map void statements

literalStatements :: List String -> List Statement
literalStatements lst = List.collect literalStatement lst

dot :: Expr -> String -> Expr
dot exp iden =
  P.Dot exp (ident iden) ()

list :: List Expr -> Expr
list exprs =
  P.List exprs ()

-- a string literal, surrounded in double quotes, because
-- language-python doesn't do that automatically
-- TODO: should we protect against double quotes within a string?
string :: String -> Expr
string st =
  P.Strings ["\"" ++ st ++ "\""] ()

int :: Int -> Expr
int val =
  P.Int (fromIntegral val) (show val) ()

ident :: String -> Ident
ident name =
  P.Ident name ()

-- variable
var :: String -> Expr
var name =
  P.Var (ident name) ()

-- function call, takes a function name and a list of arguments
call :: Expr -> List Expr -> Expr
call fnExpr args =
  P.Call
    fnExpr
    (List.map (\e -> P.ArgExpr e ()) args)
    ()

-- Python function
def :: String -> List (String, Maybe Expr, Maybe Expr) -> Maybe Expr -> List Statement -> Statement
def name params retType body =
  P.Fun (ident name) (List.map param params) retType body ()
 where
  -- function parameter with an optional type declaration
  param (paramName, typ, defaultValue) =
    P.Param (ident paramName) typ defaultValue ()

-- Assignment statement `var = expr`
set :: Expr -> Expr -> Statement
set name expr =
  P.Assign [name] expr ()

-- Python class declaration
cls :: String -> List Expr -> List Statement -> Statement
cls name args members =
  P.Class
    (ident name)
    (List.map (\a -> P.ArgExpr a ()) args)
    members
    ()

-- A static method decorator
staticmethod :: Statement -> Statement
staticmethod func =
  P.Decorated [P.Decorator [ident "staticmethod"] [] ()] func ()

-- A plus operator `a + b`
plus :: Expr -> Expr -> Expr
plus a b =
  P.BinaryOp (P.Plus ()) a b ()

-- An equality operator `a == b`
eq :: Expr -> Expr -> Expr
eq a b =
  P.BinaryOp (P.Equality ()) a b ()

-- An is operator `a is b`
is :: Expr -> Expr -> Expr
is a b =
  P.BinaryOp (P.Is ()) a b ()

-- An and operator `a and b`
and :: Expr -> Expr -> Expr
and a b =
  P.BinaryOp (P.And ()) a b ()

-- An or operator `a or b`
or :: Expr -> Expr -> Expr
or a b =
  P.BinaryOp (P.Or ()) a b ()

subscript :: Expr -> List Expr -> Expr
subscript var1 [var2] =
  P.Subscript var1 var2 ()
subscript var1 args =
  P.Subscript var1 (P.Tuple args ()) ()

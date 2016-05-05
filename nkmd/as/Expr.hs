module Expr where

data EOpType = EOpAdd | EOpSub deriving Show

data Expr
  = ExprInteger Integer
  | ExprRef String
  | ExprOp EOpType Expr Expr
  deriving Show

resolved :: Expr -> Bool
resolved (ExprInteger _) = True
resolved _ = False

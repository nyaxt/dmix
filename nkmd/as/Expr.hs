module Expr where

import Text.Printf (printf)

data EOpType = EOpAdd | EOpSub deriving Show

data Expr
  = ExprInteger Integer
  | ExprRef String
  | ExprOp EOpType Expr Expr

instance Show Expr where
  show (ExprInteger n) = printf "0x%02x=%d" n n

resolved :: Expr -> Bool
resolved (ExprInteger _) = True
resolved _ = False

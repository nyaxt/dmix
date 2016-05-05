module Program where

import Expr
import Insn

data Stmt
  = StInsn Insn
  | StLabel String
  | StConstExpr String Expr
  deriving Show

type Program = [Stmt]


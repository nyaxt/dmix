module Program where

import Expr
import Insn

data Stmt
  = StInsn Insn
  | StLabel String
  | StConst String Expr
  deriving Show

type Program = [Stmt]


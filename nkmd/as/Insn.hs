module Insn where

import Expr

data RegSel
  = Rc0 
  | Ra 
  | Rb 
  | Rc 
  | Rd 
  | Re 
  | Rf 
  | Rpc

instance Show RegSel where
  show Rc0 = "c0"
  show Ra = "a"
  show Rb = "b"
  show Rc = "c"
  show Rd = "d"
  show Re = "e"
  show Rf = "f"
  show Rpc = "PC"

data AluSel
  = OpAdd 
  | OpSub 
  | OpOr 
  | OpAnd 
  | OpXor 
  | OpReserved
  | OpClamp
  | OpMul 
  deriving Show

data AluExprT =
  AluExpr AluSel
          RegSel
          (Either RegSel Expr)

instance Show AluExprT where
  show (AluExpr alu rs (Left rt)) = 
    (show alu) ++ "(" ++ (show rs) ++ ", " ++ (show rt) ++ ")"
  show (AluExpr alu rs (Right imm)) = 
    (show alu) ++ "(" ++ (show rs) ++ ", imm " ++ (show imm) ++ ")"

modifyAlueExpr :: (Expr -> Expr) -> AluExprT -> AluExprT
modifyAlueExpr f (AluExpr asel rsel (Right e)) = AluExpr asel rsel (Right (f e))
modifyAlueExpr _ alue = alue

data MemSel
  = MNone 
  | MR 
  | MT
  | MC 
  deriving (Eq,Show)

show1 :: MemSel -> String
show1 MR = "R"
show1 MT = "T"
show1 MC = "C"
show1 MNone = "-"

data Insn =
  ArithInsn {memw :: MemSel
            ,memr :: MemSel
            ,rd :: RegSel
            ,alue :: AluExprT} |
  CntlFInsn {rd :: RegSel
            ,imm :: Expr
            -- ,linked :: Bool
            -- ,rs :: RegSel
            -- ,rt :: RegSel
            }

instance Show Insn where
  show (ArithInsn{memw = w,memr = r,rd = d,alue = a}) = 
    "ArithInsn{M[" ++
    (show1 w) ++ (show1 r) ++ "] " ++ (show d) ++ " " ++ (show a) ++ "}"
  show (CntlFInsn{rd = rd, imm = imm}) =
    "CntlFInsn{rd=" ++ (show rd) ++ ", imm=" ++ (show imm) ++ "}"

modifyInsnExpr :: (Expr -> Expr) -> Insn -> Insn
modifyInsnExpr f i@ArithInsn{} = i { alue = (modifyAlueExpr f (alue i)) }
modifyInsnExpr f i@CntlFInsn{} = i { imm = (f (imm i)) }

type Object = [Insn]

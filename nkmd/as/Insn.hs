module Insn where

import Data.Bool
import Expr

data RegSel
  = Rc0 
  | Ra 
  | Rb 
  | Rc 
  | Rd 
  | Re 
  | Rf 
  | Rg 
  | Rh
  | Ri
  | Rj
  | Rra
  | Rsl
  | Rsh
  | Rn
  | Rpc deriving (Eq)

instance Show RegSel where
  show Rc0 = "c0"
  show Ra = "a"
  show Rb = "b"
  show Rc = "c"
  show Rd = "d"
  show Re = "e"
  show Rf = "f"
  show Rg = "g"
  show Rh = "h"
  show Ri = "i"
  show Rj = "j"
  show Rra = "RA"
  show Rsl = "sl"
  show Rsh = "sh"
  show Rn = "n"
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
  deriving (Eq,Show)

data CmpSel
  = CmpNone
  | CmpEq
  | CmpGt
  | CmpAnd
  deriving (Eq,Show)

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
            ,alusel :: AluSel
            ,s :: RegSel
            ,t :: (Either RegSel Expr)
            ,memrs :: MemSel
            ,memrt :: MemSel
            ,rd :: RegSel} |
  CntlFInsn {rd :: RegSel
            ,imm :: Expr
            -- ,linked :: Bool
	    ,cmpneg :: Bool
            ,cmp :: CmpSel
            ,rs :: RegSel
            ,rt :: RegSel }

nopInsn :: Insn
nopInsn = ArithInsn { memw = MNone
                    , alusel = OpAdd
                    , s = Rc0
                    , t = Left Rc0
                    , memrs = MNone
                    , memrt = MNone
                    , rd = Rc0 }

showT :: Either RegSel Expr -> String
showT (Left l) = show l
showT (Right r) = show r

instance Show Insn where
  show (i@ArithInsn{}) = 
    "ArithInsn{M[" ++
    (show1 (memw i)) ++ (show1 (memrs i)) ++ (show1 (memrt i)) ++ "] d=" ++ (show (rd i)) ++ " alu=" ++ (show (alusel i)) ++ " s=" ++ (show (s i)) ++ " memrs=" ++ (show (memrs i)) ++ " t=" ++ (showT (t i)) ++ " memrt=" ++ (show (memrt i)) ++ "}"
  show (i@CntlFInsn{cmp = CmpNone}) =
    "JmpInsn{rd=" ++ (show (rd i)) ++ ", imm=" ++ (show (imm i)) ++ "}"
  show (i@CntlFInsn{}) =
    "CondJmpInsn{"++(bool "" "!" (cmpneg i))++(show (cmp i))++"("++(show (rs i))++", "++(show (rd i))++") rd=" ++ (show (rd i)) ++ ", imm=" ++ (show (imm i)) ++ "}"

modifyInsnExpr :: (Expr -> Expr) -> Insn -> Insn
modifyInsnExpr f i@ArithInsn{t = (Right e)} = i {t = Right (f e)}
modifyInsnExpr f i@CntlFInsn{} = i {imm = (f (imm i))}
modifyInsnExpr _ i = i

branchTargetAddr :: Insn -> Maybe Int
branchTargetAddr CntlFInsn{rd = Rc0, imm = (ExprInteger immI)} = Just (fromIntegral immI)
branchTargetAddr _ = Nothing

maybeRs :: Insn -> Maybe RegSel
maybeRs ArithInsn{s=rs} = Just rs
maybeRs _ = Nothing

maybeRt :: Insn -> Maybe RegSel
maybeRt ArithInsn{t=Left rt} = Just rt
maybeRt _ = Nothing

type Object = [Insn]

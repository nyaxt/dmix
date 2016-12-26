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
  deriving Show

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
            -- ,rs :: RegSel
            -- ,rt :: RegSel
            }

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
  show (CntlFInsn{rd = rd, imm = imm}) =
    "CntlFInsn{rd=" ++ (show rd) ++ ", imm=" ++ (show imm) ++ "}"

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

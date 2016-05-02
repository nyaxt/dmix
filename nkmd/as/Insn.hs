module Insn where

data RegSel = Rc0 | Ra | Rb | Rc | Rd | Re | Rf
instance Show RegSel where
  show Rc0 = "c0"
  show Ra = "a"
  show Rb = "b"
  show Rc = "c"
  show Rd = "d"
  show Re = "e"
  show Rf = "f"

data AluSel = OpAdd | OpSub | OpOr | OpAnd | OpXor | OpNot | OpShift deriving Show

data AluExprT = AluExpr AluSel RegSel (Either RegSel Integer)
instance Show AluExprT where
  show (AluExpr alu rs (Left rt)) = (show alu)++"("++(show rs)++", "++(show rt)++")"
  show (AluExpr alu rs (Right imm)) = (show alu)++"("++(show rs)++", imm "++(show imm)++")"

data MemSel = MNone | MR | MC

show1 :: MemSel -> String
show1 MR    = "R"
show1 MC    = "C"
show1 MNone = "-"

data Insn =
  Insn {memw :: MemSel,
        memr :: MemSel,
        rd :: RegSel,
        alue :: AluExprT }
instance Show Insn where
  show (Insn {memw = w, memr = r, rd = d, alue = a}) =
    "Insn{M["++(show1 w)++(show1 r)++"] "++
    (show d)++" "++(show a)++"}"

-- instance Show Insn where
--   show (Insn {memw = False, memr = False, dsel = dsel, alue = alue}) =
--     show dsel++" <- "++show alue
--   show (Insn {memw = False, memr = True, dsel = dsel, alue = alue}) =
--     show dsel++" <- ["++show alue++"]"
--   show (Insn {memw = True, memr = False, dsel = dsel, alue = alue}) =
--     "["++show alue++"] <- "++show dsel
--   show (Insn {memw = True, memr = True, dsel = dsel, alue = alue}) =
--     "Insn {memw = True, memr = True}" -- Error!!

type Object = [Insn]

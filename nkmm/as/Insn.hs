module Insn where

data RegSel = R0 | RegA | RegB | RegC | RegD | RegE | SP | PC 
instance Show RegSel where
  show R0 = "R0"
  show RegA = "A"
  show RegB = "B"
  show RegC = "C"
  show RegD = "D"
  show RegE = "E"
  show SP = "SP"
  show PC = "PC"

data AluSel = OpAdd | OpSub | OpOr | OpAnd | OpXor | OpNot | OpShift deriving Show

data AluExprT = AluExpr AluSel RegSel (Either RegSel Integer)
instance Show AluExprT where
  show (AluExpr alu asel (Left bsel)) = (show alu)++"("++(show asel)++", "++(show bsel)++")"
  show (AluExpr alu asel (Right imm)) = (show alu)++"("++(show asel)++", imm "++(show imm)++")"

data Insn =
  Insn {memw :: Bool,
        memr :: Bool,
        dsel :: RegSel,
        alue :: AluExprT }
instance Show Insn where
  show (Insn {memw = w, memr = r, dsel = d, alue = a}) =
    "Insn{M["++(if w then "W" else "-")++(if r then "R" else "-")++"] "++
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

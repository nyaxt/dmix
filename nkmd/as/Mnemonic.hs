module Mnemonic where

import Insn
import Data.Bits
import Data.Word (Word32)

b0 :: Word32
b0 = 0

b1 :: Word32
b1 = 1

b :: Bool -> Word32
b True = b1
b False = b0

memSelBits :: MemSel -> Word32
memSelBits MNone = 0x0
memSelBits MR = 0x1
memSelBits MC = 0x2

memwrBits :: MemSel -> MemSel -> Word32
memwrBits w r = ((memSelBits w) `shiftL` 30) .|. ((memSelBits r) `shiftL` 29)

aluSelBits :: AluSel -> Word32
aluSelBits OpAdd = 0x0
aluSelBits OpSub = 0x1
aluSelBits OpOr = 0x2
aluSelBits OpAnd = 0x3
aluSelBits OpXor = 0x4
aluSelBits OpNot = 0x5
aluSelBits OpShift = 0x6

regSelBits :: RegSel -> Word32
regSelBits Rc0 = 0x0
regSelBits Ra = 0x1
regSelBits Rb = 0x2
regSelBits Rc = 0x3
regSelBits Rd = 0x4
regSelBits Re = 0x5
regSelBits Rf = 0x6

assembleBSel :: (Either RegSel Integer) -> Word32
assembleBSel (Left bsel) = (regSelBits bsel) `shiftL` 17
assembleBSel (Right imm) = (b1 `shiftL` 16) .|. (fromInteger imm)

assembleAluE :: AluExprT -> Word32
assembleAluE (AluExpr alu asel b) = 
  ((aluSelBits alu) `shiftL` 23) .|. ((regSelBits asel) `shiftL` 20) .|.
  (assembleBSel b)

assemble :: Insn -> Word32
assemble Insn{memw = w,memr = r,rd = d,alue = alue} = 
  (memwrBits w r) .|. ((regSelBits d) `shiftL` 26) .|. assembleAluE alue

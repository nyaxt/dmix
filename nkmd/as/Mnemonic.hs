module Mnemonic where

import Insn
import Expr
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
memSelBits MT = 0x2
memSelBits MC = 0x3

memwrBits :: MemSel -> MemSel -> Word32
memwrBits MNone MNone = 0x0
memwrBits w MNone = (b1 `shiftL` 30) .|. ((memSelBits w) `shiftL` 28)
memwrBits MNone r = (b0 `shiftL` 30) .|. ((memSelBits r) `shiftL` 28)

aluSelBits :: AluSel -> Word32
aluSelBits OpAdd = 0x0
aluSelBits OpSub = 0x1
aluSelBits OpOr = 0x2
aluSelBits OpAnd = 0x3
aluSelBits OpXor = 0x4
aluSelBits OpReserved = 0x5
aluSelBits OpClamp = 0x6
aluSelBits OpMul = 0x7

regSelBits :: RegSel -> Word32
regSelBits Rc0 = 0x0
regSelBits Ra = 0x1
regSelBits Rb = 0x2
regSelBits Rc = 0x3
regSelBits Rd = 0x4
regSelBits Re = 0x5
regSelBits Rf = 0x6

immBits :: Integer -> Word32
immBits n = (b1 `shiftL` 16) .|. ((fromInteger n :: Word32) .&. 0xffff)

assemble :: Insn -> Word32
assemble i@ArithInsn{alue = (AluExpr asel rs (Right (ExprInteger n)))} = (memwrBits (memw i) (memr i)) .|. ((regSelBits (rd i)) `shiftL` 24) .|. ((aluSelBits asel) `shiftL` 21) .|. (immBits n)
assemble i@CntlFInsn{imm = (ExprInteger n)} = (b1 `shiftL` 31) .|. ((regSelBits (rd i)) `shiftL` 24) .|. (immBits n)
assemble _ = 0xdead

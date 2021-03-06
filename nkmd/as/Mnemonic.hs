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

jmpEn :: Word32
jmpEn = b1 `shiftL` 31

cmpEn :: Word32
cmpEn = b1 `shiftL` 30

cmpBits :: CmpSel -> Word32
cmpBits CmpNone = 0xdead
cmpBits CmpEq = 0x0
cmpBits CmpGt = 0x1
cmpBits CmpAnd = 0x2

memSelBits :: MemSel -> Word32
memSelBits MNone = 0x0
memSelBits MR = 0x1
memSelBits MT = 0x2
memSelBits MC = 0x3

memwBits :: MemSel -> Word32
memwBits w = (memSelBits w) `shiftL` 29

memrsBits :: MemSel -> Word32
memrsBits MNone = 0x0
memrsBits MC = (b1 `shiftL` 0)
memrsBits _ = 0xdead

memrtBits :: MemSel -> Word32
memrtBits MNone = 0x0
memrtBits MR = (b1 `shiftL` 28)
memrtBits MT = (b1 `shiftL` 28) .|. (b1 `shiftL` 1)
memrtBits _ = 0xdead

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
regSelBits Rg = 0x7
regSelBits Rh = 0x8
regSelBits Ri = 0x9
regSelBits Rj = 0xa
regSelBits Rra = 0xb
regSelBits Rsl = 0xc
regSelBits Rsh = 0xd
regSelBits Rn = 0xe
regSelBits Rpc = 0xf

-- FIXME; handle neg
immBits :: Integer -> Word32
immBits n = (b1 `shiftL` 16) .|. ((fromInteger n :: Word32) .&. 0xffff)

assembleCommon :: Insn -> Word32
assembleCommon i@ArithInsn{} =
    (memwBits (memw i)) .|.
    (memrsBits (memrs i)) .|.
    (memrtBits (memrt i)) .|.
    ((regSelBits (rd i)) `shiftL` 24) .|.
    ((regSelBits (s i)) `shiftL` 17) .|.
    ((aluSelBits (alusel i)) `shiftL` 21)

assemble :: Insn -> Word32
assemble i@ArithInsn{t = Right (ExprInteger n)} =
    (assembleCommon i) .|.
    (immBits n)
assemble i@ArithInsn{t = Left rt} =
    (assembleCommon i) .|.
    ((regSelBits rt) `shiftL` 8)
assemble i@CntlFInsn{cmp = CmpNone, imm = (ExprInteger n)} =
    jmpEn .|.
    ((regSelBits (rd i)) `shiftL` 24) .|.
    (immBits n)
assemble i@CntlFInsn{imm = (ExprInteger n)} | (n > 0 && n <= 0xff) =
    jmpEn .|. cmpEn .|.
    ((regSelBits (rd i)) `shiftL` 24) .|.
    ((cmpBits (cmp i)) `shiftL` 22) .|. ((b (cmpneg i)) `shiftL` 21) .|.
    ((regSelBits (rs i)) `shiftL` 17) .|.
    ((regSelBits (rt i)) `shiftL` 8) .|.
    (immBits n) -- FIXME: need immBits8
assemble _ = 0xdeadbeef

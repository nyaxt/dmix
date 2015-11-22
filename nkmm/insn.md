# NKMM CPU Instruction Set

## Registers
* R0-R5: General purpose registers ACCUM_WIDTH-bit
* SP: Stack pointer ADDR_WIDTH-bit
* PC: Program counter ADDR_WIDTH-bit

## General
REG_D <- ALU[ALUSEL](REG_A, REG_B or IMM)
or
REG_D <- MEM(ALU[ALUSEL](REG_A, REG_B or IMM))
or
MEM(ALU[ALUSEL](REG_A, REG_B or IMM)) <- REG_D

## Load
REG <- 0 + REG
REG <- 0 + IMM8

## Fetch
REG <- [ALU]

## Store
[ALU] <- REG

## Add
REG <- REG + REG
REG <- REG + IMM8

## Shift L/R
REG <- REG << {1,2,4,8,16}

## MulAdd
FMULADD [R1++] [R2++]

pseudocode:

```
R0 += [R1] * [R2];
if (R1++FLAG) R1 = R1 + 1;
if (R2++FLAG) R2 = R2 + 1;
R6 = R6 - 1;
```

## REPZ
REPZ INSN

execute INSN iff R6 != 0

## Jump
PC <- PC + IMM8
PC <- PC - IMM8
PC <- REG
PC <- [REG]

## Instruction Decoded
- Conditional Execution 3bit
 - EQ ==
 - NE !=
 - GE >=
 - LT <
 - GT >
 - LE <=
 - ALways
 - Counter Zero?
- Destination Register
 - None (ST update only)
 - GPR
 - PC (Jump)
- ALU Input Register ALU_A
 - 4'h0 - 4'hD  GPR
 - 4'hE PC
 - 4'hF Zero
- ALU Input Register ALU_B
 - 4'h0 - 4'hD GPR
 - 4'hE undef
 - 4'hF Immediate
- ALU Operator Selection
 * 3'b000 Add
 * 3'b001 Sub
 * 3'b010 bit OR
 * 3'b011 bit AND
 * 3'b100 bit XOR
 * 3'b101 bit NOT
 * 3'b110 SHIFT
 * 3'b111 MUL?
- ALU Use Carry/Borrow?
- Memory Operaton
 - Read
 - Write
- Immediate Value
- Sign Extend
- Decrement Counter
- HALT until INTR
 - I/O

## Processor Pipeline

### IF
I <- [PC]
PC <- PC+4

### DECODE
{ALU_SEL, REG_SEL_A, REG_SEL_B, ...} <- DECODE(PC)
ALU_A <- REGMUX_A(REG_SEL_A, {R...})
ALU_B <- REGMUX_B(REG_SEL_B, {R...})

### EXECUTE
ALU_R <- ALU(ALU_A, ALU_B, IMM8)

### MEM
```
IO_DATA_O <- ALU_R
if (MWR)
  IO_WR <- 1

if (MRD)
  MEM_R <- IO_DATA_R
else
  MEM_R <- ALU_R
```

### WRITE BACK
REGMUX_D <- MEM_R

## FMULADD Pipeline



## Instruction Encoding
```
31   30  29  28 27 26  25 24 23    22 21 20  19 18 17  16    15        0
REPZ_MWR_MRD_DSEL[2:0]_ALUSEL[2:0]_ASEL[2:0]_BSEL[2:0]_IMMEN_IMM16[15:0]
```

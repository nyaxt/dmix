# NKMM CPU Instruction Set

## Registers
* R0-R6: General purpose registers ACCUM_WIDTH-bit
* PC: Program counter ADDR_WIDTH-bit

## Load
REG <- 0 + REG
REG <- 0 + IMM8

## Fetch
REG <- [REG]

## Store
[REG] <- REG

## Add
REG <- REG + REG
REG <- REG + IMM8

## Shift L/R
REG <- REG << {1,2,4,8}

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

## Instruction Encoding

```
16 + 7bit

31   30  29 28 27  26 25 24    23 22 21  20 19 18  17    15     0
REPZ_MWR_DSEL[2:0]_ALUSEL[2:0]_ASEL[2:0]_BSEL[2:0]_IMMEN_IMM8[7:0]

# EXECUTE
ALU_R <- ALU(R_A, R_B, IMM8)

# MEM
if (MWR)
  IO_WR <- 1
  IO_DATA_O <- ALU_R
else if (MRD)
  MEM_R <- IO_DATA_R
else
  MEM_R <- ALU_R

# WRITE BACK
R_D <- MEM_R
```

### ALUSEL
* 3'b000 Add
* 3'b001 Sub
* 3'b010 bit OR
* 3'b011 bit AND
* 3'b100 bit XOR
* 3'b101 bit NOT
* 3'b110 SHIFT 

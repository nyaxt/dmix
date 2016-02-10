module Main(main) where

import Text.ParserCombinators.Parsec

data RegSel = R0 | R1 | R2 | R3 | R4 | R5 | SP | PC deriving Show
data ALUSel = OpAdd | OpSub | OpOr | OpAnd | OpXor | OpNot | OpShift deriving Show

data Insn =
  Insn {memw :: Bool,
        memr :: Bool,
        dsel :: RegSel,
        alu :: ALUSel,
        asel :: RegSel,
        bsel :: RegSel,
        imm :: Maybe Int} deriving Show
type Object = [Insn]

regsel :: GenParser Char st RegSel
regsel = do x <- string "R1"
            return R1
         <?> "register"

insn :: GenParser Char st Insn
insn = 
  let memw = False
      memr = False
      alu = OpAdd
      asel = R5
      bsel = R5
      imm = Just 42
  in do dsel <- regsel
        return Insn { memw = memw, memr = memr, dsel = dsel, alu = alu, asel = asel, bsel = bsel, imm = imm }
     <?> "instruction"

nkmm :: GenParser Char st Object
nkmm = do is <- many insn
          eof
          return is

parseNkmmAs :: String -> Either ParseError Object
parseNkmmAs = parse nkmm "(unknown)"

main :: IO ()
main = do src <- getContents
          putStrLn $ show $ parseNkmmAs src

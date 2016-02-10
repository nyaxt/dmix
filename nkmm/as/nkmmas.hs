module Main(main) where

import Parser
import Insn

import Numeric (showHex, showIntAtBase)
import System.IO
import Data.List
import Data.Word (Word32)
import Data.Bits
-- import Options.Applicative

assemble :: Insn -> Word32 
assemble Insn {memw = w, memr = r, dsel = d, alue = (AluExpr alu asel (Left bsel))} =
  1234
assemble Insn {memw = w, memr = r, dsel = d, alue = (AluExpr alu asel (Right imm))} =
  4321

showObj :: Object -> String
showObj obj = intercalate "\n" $ map show obj

processInsn :: Insn -> String
processInsn insn =
  "// "++ show insn ++ "\n"++
  "16'h"++(showHex 123 "")++": data_ff <= 32'h"++(showHex (assemble insn) "")++";\n"

processObj :: Object -> String
processObj = concatMap processInsn

main :: IO ()
main = do src <- getContents
	  case (parseNkmmAs src) of
	    (Left err) -> hPutStrLn stderr $ show err
	    (Right obj) -> putStr $ processObj obj 

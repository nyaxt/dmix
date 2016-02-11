module Main(main) where

import Parser
import Insn
import Mnemonic (assemble)

import Text.Printf (printf)
import Data.List
import Data.Word (Word, Word32)
import System.IO
-- import Options.Applicative

showObj :: Object -> String
showObj obj = intercalate "\n" $ map show obj

processInsn :: Word -> Insn -> String
processInsn addr insn =
  let asm = assemble insn
  in printf "// %s\n16'h%04x: data_ff <= 32'h%08x;\n" (show insn) addr asm

processObj :: Word -> Object -> String
processObj startAddr is = concat $ map (uncurry processInsn) $ zip [startAddr..] is

main :: IO ()
main = do src <- getContents
	  case (parseNkmmAs src) of
	    (Left err) -> hPutStrLn stderr $ show err
	    (Right obj) -> putStr $ processObj 0 obj 

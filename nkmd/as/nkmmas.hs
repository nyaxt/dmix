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

processObjBody :: Word -> Object -> String
processObjBody startAddr is = concat $ map (uncurry processInsn) $ zip [startAddr..] is

headerStr = unlines [
  "`include \"../nkmm_const.v\"",
  "module nkmm_progrom(",
  "    input clk,",
  "    ",
  "    output [`INSN_WIDTH-1:0] prog_data_o,",
  "    input [`ADDR_WIDTH-1:0] prog_addr_i);",
  "",
  "reg [`INSN_WIDTH-1:0] data_ff;",
  "",
  "always @(posedge clk) begin",
  "    case (prog_addr_i)",
  "// **** HEADER END ****"]

footerStr = unlines [
  "// **** FOOTER BEGIN ****",
  "// Insn{M[--] D OpAdd(R0, imm 0)}",
  "default: data_ff <= 32'h10010000;",
  "    endcase",
  "end",
  "",
  "assign prog_data_o = data_ff;",
  "",
  "endmodule"]

processObj :: Object -> String
processObj obj = headerStr ++ objBody ++ footerStr
  where objBody = processObjBody 0 obj

main :: IO ()
main = do src <- getContents
	  case (parseNkmmAs src) of
	    (Left err) -> hPutStrLn stderr $ show err
	    (Right obj) -> putStr $ processObj obj

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main(main) where

import Parser
import Insn
import Mnemonic (assemble)

import Control.Monad.State
import Control.Applicative
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
processObj obj = objBody -- headerStr ++ objBody ++ footerStr
  where objBody = processObjBody 0 obj

type Offset = Int
data LabelInfo = LabelInfo { name :: String, offset :: Offset }

data PreprocessorState = PreprocessorState { currOffset :: Offset, labels :: [LabelInfo] }
initialPreprocessorState :: PreprocessorState
initialPreprocessorState = PreprocessorState { currOffset = 0, labels = [] }

newtype Preprocessor a = Preprocessor { runPreprocessor :: State PreprocessorState a }
  deriving (Functor, Applicative, Monad, MonadState PreprocessorState)

incrOffset :: Int -> Preprocessor ()
incrOffset n = modify $ \s -> s { currOffset = (currOffset s) + 1 }

preprocessStmt :: Stmt -> Preprocessor ()
preprocessStmt (StInsn _) = do
  incrOffset 1 
  return ()

preprocessStmt (StLabel name) = do
  return ()

execPreprocessor :: Preprocessor a -> PreprocessorState
execPreprocessor m = execState (runPreprocessor m) initialPreprocessorState

preprocessProg :: Program -> Either String PreprocessorState
preprocessProg prog = Right $ execPreprocessor $ mapM_ preprocessStmt prog

data CompilerState = CompilerState { compiledObj :: Object }
initialCompilerState :: CompilerState
initialCompilerState = CompilerState { compiledObj = [] }

newtype Compiler a = Compiler { runCompiler :: State CompilerState a }
  deriving (Functor, Applicative, Monad, MonadState CompilerState)

compileStmt :: Stmt -> Compiler ()
compileStmt (StInsn insn) = do
  modify $ \s -> s { compiledObj = (compiledObj s) ++ [insn] }
  return ()

compileStmt (StLabel _) = return ()

execCompiler :: Compiler a -> CompilerState
execCompiler m = execState (runCompiler m) initialCompilerState

compileProg :: PreprocessorState -> Program -> Either String Object
compileProg prep prog = Right $ compiledObj $ execCompiler $ mapM_ compileStmt prog

handleProgram :: Program -> IO ()
handleProgram prog = do
  case (preprocessProg prog) of
    (Left err) -> hPutStrLn stderr $ show err
    (Right prep) ->
      case (compileProg prep prog) of
        (Left err) -> hPutStrLn stderr $ show err
        (Right obj) -> putStr $ processObj obj

main :: IO ()
main = do src <- getContents
	  case (parseNkmmAs src) of
	    (Left err) -> hPutStrLn stderr $ show err
	    (Right prog) -> handleProgram prog

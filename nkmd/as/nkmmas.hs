{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Expr
import Parser
import Insn
import Program
import Mnemonic (assemble)
import Control.Monad.State
import Control.Applicative
import Text.Printf (printf)
import Data.Either.Unwrap (fromRight)
import Data.List
import qualified Data.Map.Strict as Map
import Data.Word (Word, Word32)
import System.IO

-- import Options.Applicative
--
showObj :: Object -> String
showObj obj = intercalate "\n" $ map show obj

processInsn :: Word -> Insn -> String
processInsn addr insn = 
  let asm = assemble insn
  in printf "// %s\n16'h%04x: data_ff <= 32'h%08x;\n" (show insn) addr asm

processObjBody :: Word -> Object -> String
processObjBody startAddr is = 
  concat $ map (uncurry processInsn) $ zip [startAddr ..] is

headerStr = 
  unlines ["`include \"../nkmm_const.v\""
          ,"module nkmm_progrom("
          ,"    input clk,"
          ,"    "
          ,"    output [`INSN_WIDTH-1:0] prog_data_o,"
          ,"    input [`ADDR_WIDTH-1:0] prog_addr_i);"
          ,""
          ,"reg [`INSN_WIDTH-1:0] data_ff;"
          ,""
          ,"always @(posedge clk) begin"
          ,"    case (prog_addr_i)"
          ,"// **** HEADER END ****"]

footerStr = 
  unlines ["// **** FOOTER BEGIN ****"
          ,"// Insn{M[--] D OpAdd(R0, imm 0)}"
          ,"default: data_ff <= 32'h10010000;"
          ,"    endcase"
          ,"end"
          ,""
          ,"assign prog_data_o = data_ff;"
          ,""
          ,"endmodule"]

processObj :: Object -> String
processObj obj = objBody -- headerStr ++ objBody ++ footerStr
  where objBody = processObjBody 0 obj

type Offset = Int

data LabelInfo =
  LabelInfo {name :: String
            ,offset :: Offset}
  deriving (Show)

type ConstExprMap = Map.Map String Expr

data PreprocessorState =
  PreprocessorState {currOffset :: Offset
                    ,labels :: [LabelInfo]
                    ,constexprs :: ConstExprMap}
  deriving (Show)

initialPreprocessorState :: PreprocessorState
initialPreprocessorState = 
  PreprocessorState {currOffset = 0
                    ,labels = []
                    ,constexprs = Map.empty}

newtype Preprocessor a =
  Preprocessor {runPreprocessor :: State PreprocessorState a}
  deriving (Functor,Applicative,Monad,MonadState PreprocessorState)

incrOffset :: Int -> Preprocessor ()
incrOffset n = modify $ \s -> s {currOffset = (currOffset s) + 1}

addLabel :: LabelInfo -> Preprocessor ()
addLabel l = modify $ \s -> s {labels = l : (labels s)}

modifyConstExprs
  :: (ConstExprMap -> ConstExprMap) -> Preprocessor ()
modifyConstExprs f = modify $ \s -> s {constexprs = f (constexprs s)}

insertConstExpr
  :: String -> Expr -> Preprocessor ()
insertConstExpr n e = modifyConstExprs $ Map.insert n e

preprocessStmt :: Stmt -> Preprocessor ()
preprocessStmt (StInsn _) = incrOffset 1
preprocessStmt (StLabel name) = 
  do s <- get
     let newl = 
           LabelInfo {name = name
                     ,offset = (currOffset s)}
       in addLabel newl
preprocessStmt (StConstExpr name expr) = insertConstExpr name expr

execPreprocessor
  :: Preprocessor a -> PreprocessorState
execPreprocessor m = execState (runPreprocessor m) initialPreprocessorState

preprocessProg
  :: Program -> Either String PreprocessorState
preprocessProg prog = Right $ execPreprocessor $ mapM_ preprocessStmt prog

resolveExpr
  :: PreprocessorState -> Expr -> Either String Expr
resolveExpr p (ExprRef n) = 
  let ces = constexprs p
      lu = Map.lookup n ces
  in case lu of
       Nothing -> Left $ printf "Failed to resolve constexpr ref \"%s\"" n
       Just e -> resolveExpr p e
resolveExpr _ e
  | (resolved e) = Right e
  | otherwise = Left $ printf "failed to resolve constexpr: %s" (show e)

data CompilerState =
  CompilerState {preprocessorState :: PreprocessorState
                ,compiledObj :: Object}

initialCompilerState
  :: PreprocessorState -> CompilerState
initialCompilerState prep = 
  CompilerState {preprocessorState = prep
                ,compiledObj = []}

newtype Compiler a =
  Compiler {runCompiler :: State CompilerState a}
  deriving (Functor,Applicative,Monad,MonadState CompilerState)

appendInsn :: Insn -> Compiler ()
appendInsn insn = modify $ \s -> s {compiledObj = (compiledObj s) ++ [insn]}

compileStmt :: Stmt -> Compiler ()
compileStmt (StInsn insn) = 
  do prep <- gets preprocessorState
     appendInsn $
       modifyInsnExpr (fromRight . (resolveExpr prep))
                      insn
compileStmt _ = return ()

execCompiler
  :: PreprocessorState -> Compiler a -> CompilerState
execCompiler prep m = 
  execState (runCompiler m)
            (initialCompilerState prep)

compileProg
  :: PreprocessorState -> Program -> Either String Object
compileProg prep prog = 
  Right $ compiledObj $ execCompiler prep $ mapM_ compileStmt prog

handleProgram :: Program -> IO ()
handleProgram prog = 
  case (preprocessProg prog) of
    (Left err) -> hPutStrLn stderr $ show err
    (Right prep) -> 
      do hPutStrLn stderr $ show prep
         case (compileProg prep prog) of
           (Left err) -> hPutStrLn stderr $ show err
           (Right obj) -> putStr $ processObj obj

main :: IO ()
main = 
  do src <- getContents
     case (parseNkmmAs src) of
       (Left err) -> hPutStrLn stderr $ show err
       (Right prog) -> handleProgram prog

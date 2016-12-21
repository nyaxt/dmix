{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Expr
import Parser
import Insn
import Program
import Preprocessor
import Mnemonic (assemble)
import Output

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Reader
import Data.Either.Unwrap (fromRight)
import Data.List
import Data.Maybe
import Data.Word (Word, Word32)
import Data.Semigroup ((<>))
import Options.Applicative
import System.IO
import Text.Printf (printf)

data Options = Options
  { showMnemonic :: Bool
  , outputFormat :: OutputFormat
  } deriving Show

optionsI :: ParserInfo Options
optionsI = info (helper <*> optionsP) $ progDesc "Nkmd CPU assembler"
  where
    showMneumonicP :: Parser Bool
    showMneumonicP = switch $ short 'm' <> long "show-mnemonic" <> help "Show mnemonic info (for suported output formats only)"

    outputFormatP :: Parser OutputFormat
    outputFormatP = option auto $ long "format" <> help "Output file format" <> value Progrom <> showDefault

    optionsP :: Parser Options
    optionsP = Options <$> showMneumonicP <*> outputFormatP

showObj :: Object -> String
showObj obj = intercalate "\n" $ map show obj

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

doPreprocess :: Program -> IO (Maybe PreprocessorState)
doPreprocess prog = case (preprocessProg prog) of
    (Left err) -> do hPutStrLn stderr $ show err
                     return Nothing
    (Right prep) -> return (Just prep)

doCompile :: PreprocessorState -> Program -> IO (Maybe Object)
doCompile prep prog = case (compileProg prep prog) of
    (Left err) -> do hPutStrLn stderr $ show err
                     return Nothing
    (Right obj) -> return (Just obj)

main :: IO ()
main = do opts <- execParser optionsI
          src <- getContents
          prog <- doParse src
          guard (isJust prog)
          prep <- doPreprocess (fromJust prog)
          guard (isJust prep)
          obj <- doCompile (fromJust prep) (fromJust prog)
          guard (isJust obj)
          putStr $ outputObj (outputFormat opts) (fromJust obj)

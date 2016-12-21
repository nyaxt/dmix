{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler (compileProg) where

import Insn
import Preprocessor
import Program

import Data.Either.Unwrap (fromRight)
import Control.Monad.State

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

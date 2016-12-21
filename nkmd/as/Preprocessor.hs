{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Preprocessor (PreprocessorState, preprocessProg, resolveExpr) where

import Expr
import Program

import Control.Monad.State
import Text.Printf (printf)
import qualified Data.Map.Strict as Map

type Offset = Int

type ConstExprMap = Map.Map String Expr

data PreprocessorState =
  PreprocessorState {currOffset :: Offset
                    ,constexprs :: ConstExprMap}
  deriving (Show)

initialPreprocessorState :: PreprocessorState
initialPreprocessorState = 
  PreprocessorState {currOffset = 0
                    ,constexprs = Map.empty}

newtype Preprocessor a =
  Preprocessor {runPreprocessor :: State PreprocessorState a}
  deriving (Functor,Applicative,Monad,MonadState PreprocessorState)

incrOffset :: Int -> Preprocessor ()
incrOffset n = modify $ \s -> s {currOffset = (currOffset s) + 1}

modifyConstExprs
  :: (ConstExprMap -> ConstExprMap) -> Preprocessor ()
modifyConstExprs f = modify $ \s -> s {constexprs = f (constexprs s)}

insertConstExpr
  :: String -> Expr -> Preprocessor ()
insertConstExpr n e = modifyConstExprs $ Map.insert n e

preprocessStmt :: Stmt -> Preprocessor ()
preprocessStmt (StInsn _) = incrOffset 1
preprocessStmt (StLabel name) = 
  do coff <- gets currOffset
     insertConstExpr name (ExprInteger (toInteger coff))
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

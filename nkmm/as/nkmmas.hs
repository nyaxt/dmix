module Main(main) where

import Parser
import Insn

import System.IO
import Data.List

showObj :: Object -> String
showObj obj = intercalate "\n" $ map show obj

main :: IO ()
main = do src <- getContents
	  case (parseNkmmAs src) of
	    (Left err) -> hPutStrLn stderr $ show err
	    (Right obj) -> putStrLn $ showObj obj 

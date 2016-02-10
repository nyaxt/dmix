module Main(main) where

import Control.Monad
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

data RegSel = R0 | RegA | RegB | RegC | RegD | RegE | SP | PC deriving Show
data ALUSel = OpAdd | OpSub | OpOr | OpAnd | OpXor | OpNot | OpShift deriving Show

data Insn =
  Insn {memw :: Bool,
        memr :: Bool,
        dsel :: RegSel,
        alu :: ALUSel,
        asel :: RegSel,
        bsel :: Either RegSel Integer} deriving Show
type Object = [Insn]

type AluExpr = (ALUSel, RegSel, Either RegSel Integer)

lexer :: P.TokenParser ()
lexer = P.makeTokenParser style
  where style = emptyDef {
    P.reservedOpNames = ["<-", "+", "-", "|", "&", "^", "!", ";"],
    P.reservedNames = ["R0", "R1", "R2", "R3", "R4", "R5", "SP", "PC"],
    P.commentLine = "#" }

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

regSel :: Parser RegSel
regSel = (reserved "R0" >> return R0)
       <|> (reserved "A" >> return RegA)
       <|> (reserved "B" >> return RegB)
       <|> (reserved "C" >> return RegC)
       <|> (reserved "D" >> return RegD)
       <|> (reserved "E" >> return RegE)
       <|> (reserved "SP" >> return SP)
       <|> (reserved "PC" >> return PC)
       <?> "register"

imm :: Parser Integer
imm = P.natural lexer

regImm :: Parser (Either RegSel Integer)
regImm = liftM Left regSel <|> liftM Right imm

loadExpr :: Parser AluExpr
loadExpr = do bsel <- regImm
              return (OpAdd, R0, bsel)
           <?> "loadExpr"

notExpr = do reservedOp "!"
             bsel <- regImm
             return (OpNot, R0, bsel)
           <?> "notExpr"


makeAluExpr :: (Parser ()) -> ALUSel -> Parser AluExpr
makeAluExpr opP alu = do asel <- regSel
	                 opP
                         bsel <- regImm
                         return (alu, asel, bsel)
                      <?> "aluExpr"

aluExpr :: Parser AluExpr
aluExpr = (try $ makeAluExpr (reservedOp "+") OpAdd)
        <|> (try $ makeAluExpr (reservedOp "-") OpSub)
        <|> (try $ makeAluExpr (reservedOp "|") OpOr)
        <|> (try $ makeAluExpr (reservedOp "&") OpAnd)
        <|> (try $ makeAluExpr (reservedOp "^") OpXor)
        <|> notExpr
        <|> loadExpr

insn :: Parser Insn
insn = 
  let memw = False
      memr = False
      alu = OpAdd
      imm = Just 42
  in do dsel <- regSel
        reservedOp "<-"
	(alu, asel, bsel) <- aluExpr
        reservedOp ";"
        return Insn { memw = memw, memr = memr, dsel = dsel, alu = alu, asel = asel, bsel = bsel }
     <?> "instruction"
 
nkmm :: Parser Object
nkmm = do is <- many1 insn
          eof
          return is

parseNkmmAs :: String -> Either ParseError Object
parseNkmmAs = parse nkmm "(unknown)"

main :: IO ()
main = do src <- getContents
          putStrLn $ show $ parseNkmmAs src

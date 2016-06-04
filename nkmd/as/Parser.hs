module Parser (parseNkmmAs) where

import Expr
import Insn
import Program
import Control.Monad
import Data.Functor
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

lexer :: P.TokenParser ()
lexer = P.makeTokenParser style
  where style = 
          emptyDef {P.reservedOpNames = 
                      ["=","+","-","|","&","^","!",";","[","]",";"]
                   ,P.reservedNames = ["R","C","c0","a","b","c","d","e","f","const","jmp"]
                   ,P.commentLine = "#"}

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

identifier :: Parser String
identifier = P.identifier lexer

exprInteger :: Parser Expr
exprInteger =
  do v <- P.natural lexer
     return (ExprInteger v)
    
exprRef :: Parser Expr
exprRef =
  do i <- identifier
     return (ExprRef i)

expr :: Parser Expr
expr =
  (try exprInteger) <|>
  (try exprRef)

regSel :: Parser RegSel
regSel = 
  (reserved "c0" >> return Rc0) <|>
  (reserved "a" >> return Ra) <|>
  (reserved "b" >> return Rb) <|>
  (reserved "c" >> return Rc) <|>
  (reserved "d" >> return Rd) <|>
  (reserved "e" >> return Re) <|>
  (reserved "f" >> return Rf) <|>
  (reserved "g" >> return Rg) <|>
  (reserved "h" >> return Rh) <|>
  (reserved "i" >> return Ri) <?> "register"

-- imm :: Parser Integer
-- imm = P.natural lexer

regImm :: Parser (Either RegSel Expr)
regImm = liftM Left regSel <|> liftM Right expr

{-
loadExpr :: Parser AluExprT
loadExpr =
  do bsel <- regImm
     return $ AluExpr OpAdd Rc0 bsel
  <?> "loadExpr"

notExpr = 
  do reservedOp "!"
     bsel <- regImm
     return $ AluExpr OpXor Rc0 bsel
  <?> "notExpr"

makeAluExpr
  :: (Parser ()) -> AluSel -> Parser AluExprT
makeAluExpr opP alu = 
  do asel <- regSel
     opP
     bsel <- regImm
     return $ AluExpr alu asel bsel
  <?> "aluExpr"

aluExpr :: Parser AluExprT
aluExpr = 
  (try $
   makeAluExpr (reservedOp "+")
               OpAdd) <|>
  (try $
   makeAluExpr (reservedOp "-")
               OpSub) <|>
  (try $
   makeAluExpr (reservedOp "|")
               OpOr) <|>
  (try $
   makeAluExpr (reservedOp "&")
               OpAnd) <|>
  (try $
   makeAluExpr (reservedOp "^")
               OpXor) <|>
  notExpr <|>
  loadExpr
-}

aluSel :: Parser AluSel
aluSel = (reservedOp "+" >> return OpAdd) <|>
         (reservedOp "-" >> return OpSub) <|>
         (reservedOp "|" >> return OpOr) <|>
         (reservedOp "&" >> return OpAnd) <|>
         (reservedOp "^" >> return OpXor)

memSel :: Parser MemSel
memSel = (reserved "R" >> return MR) <|> (reserved "T" >> return MT) <|> (reserved "C" >> return MC)

memLBracket :: Parser MemSel
memLBracket = 
  do m <- memSel
     reservedOp "["
     return m

arithInsn :: Parser Insn
arithInsn = 
  do memw <- option MNone memLBracket
     rd <- regSel
     when (memw /= MNone)
          (reservedOp "]")
     reservedOp "="
     memrs <- option MNone memLBracket
     s <- regSel
     when (memrs /= MNone)
          (reservedOp "]")
     alusel <- aluSel
     memrt <- option MNone memLBracket
     t <- regImm
     when (memrt /= MNone)
          (reservedOp "]")
     reservedOp ";"
     return ArithInsn {memw = memw
                      ,alusel = alusel
		      ,s = s
		      ,t = t
                      ,memrs = memrs
                      ,memrt = memrt
                      ,rd = rd}

jmpInsn :: Parser Insn
jmpInsn =
  do reserved "jmp"
     e <- expr
     reservedOp ";"
     return CntlFInsn {rd = Rc0
                      ,imm = e}

insn :: Parser Insn
insn = choice [arithInsn,jmpInsn]

labelp :: Parser Stmt
labelp = 
  do l <- identifier
     reservedOp ":"
     return (StLabel l)

constp :: Parser Stmt
constp =
  do reserved "const"
     n <- identifier
     reservedOp "="
     e <- expr
     reservedOp ";"
     return (StConstExpr n e)

stmt :: Parser Stmt
stmt = choice [(liftM StInsn insn), labelp, constp]

nkmm :: Parser Program
nkmm =
  do is <- many1 stmt
     eof
     return is

parseNkmmAs
  :: String -> Either ParseError Program
parseNkmmAs = parse nkmm "(unknown)"

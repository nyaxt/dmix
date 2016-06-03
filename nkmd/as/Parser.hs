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
  (reserved "c0" >> return Rc0) <|> (reserved "a" >> return Ra) <|>
  (reserved "b" >> return Rb) <|>
  (reserved "c" >> return Rc) <|>
  (reserved "d" >> return Rd) <|>
  (reserved "e" >> return Re) <|>
  (reserved "c" >> return Rf) <?> "register"

-- imm :: Parser Integer
-- imm = P.natural lexer

regImm :: Parser (Either RegSel Expr)
regImm = liftM Left regSel <|> liftM Right expr

loadExpr :: Parser AluExprT
loadExpr =
  do bsel <- regImm
     return $ AluExpr OpAdd Rc0 bsel
  <?> "loadExpr"

notExpr = 
  do reservedOp "!"
     bsel <- regImm
     return $ AluExpr OpNot Rc0 bsel
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

memSel :: Parser MemSel
memSel = (reserved "R" >> return MR) <|> (reserved "C" >> return MC)

memrLBracket :: Parser MemSel
memrLBracket = 
  do m <- memSel
     reservedOp "["
     return m

rAssignInsn :: Parser Insn
rAssignInsn = 
  do rd <- regSel
     reservedOp "="
     memr <- option MNone memrLBracket
     alue <- aluExpr
     when (memr /= MNone)
          (reservedOp "]")
     reservedOp ";"
     return ArithInsn {memr = memr
                      ,memw = MNone
                      ,rd = rd
                      ,alue = alue}

mAssignInsn :: Parser Insn
mAssignInsn = 
  do memw <- memrLBracket
     alue <- aluExpr
     reservedOp "]"
     reservedOp "="
     rd <- regSel
     reservedOp ";"
     return ArithInsn {memr = MNone
                      ,memw = memw
                      ,rd = rd
                      ,alue = alue}

jmpInsn :: Parser Insn
jmpInsn =
  do reserved "jmp"
     e <- expr
     reservedOp ";"
     return CntlFInsn {rd = Rc0
                      ,imm = e}

insn :: Parser Insn
insn = choice [rAssignInsn,mAssignInsn,jmpInsn]

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

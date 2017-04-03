module Parser (parseNkmmAs, doParse) where

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
import System.IO

lexer :: P.TokenParser ()
lexer = P.makeTokenParser style
  where style = 
          emptyDef {P.reservedOpNames = 
                      ["=","+","-","|","&","^","!",";","[","]",";","(",")","=="]
                   ,P.reservedNames = ["R","C","c0","a","b","c","d","e","f","g","h","i","j","ra","sl","sh","n","pc","const","jmp","nop","if"]
                   ,P.commentLine = "#"}

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

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
  (reserved "i" >> return Ri) <|> 
  (reserved "j" >> return Rj) <|> 
  (reserved "ra" >> return Rra) <|> 
  (reserved "sl" >> return Rsl) <|> 
  (reserved "sh" >> return Rsh) <|> 
  (reserved "n" >> return Rn) <|> 
  (reserved "pc" >> return Rpc) 
  <?> "register"

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

nopInsnP :: Parser Insn
nopInsnP =
  do reserved "nop"
     reservedOp ";"
     return nopInsn

assignInsnP :: Parser Insn
assignInsnP =
  do memw <- option MNone memLBracket
     rd <- regSel
     when (memw /= MNone)
          (reservedOp "]")
     reservedOp "="
     memrt <- option MNone memLBracket
     t <- regImm
     when (memrt /= MNone)
          (reservedOp "]")
     reservedOp ";"
     return ArithInsn {memw = memw
                      ,alusel = OpAdd
                      ,s = Rc0
                      ,t = t
                      ,memrs = MNone
                      ,memrt = memrt
                      ,rd = rd}

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

condJmpInsn :: Parser Insn
condJmpInsn =
  do reserved "if"
     reservedOp "("
     s <- regSel
     reservedOp "=="
     t <- regSel
     reservedOp ")"
     reserved "jmp"
     e <- expr
     reservedOp ";"
     return CntlFInsn {rd = Rc0
                      ,imm = e
                      ,cmp = CmpEq
                      ,cmpneg = False
                      ,rs = s
                      ,rt = t}

jmpInsn :: Parser Insn
jmpInsn =
  do reserved "jmp"
     e <- expr
     reservedOp ";"
     return CntlFInsn {rd = Rc0
                      ,imm = e
                      ,cmp = CmpNone
                      ,cmpneg = False
                      ,rs = Rc0
                      ,rt = Rc0}

insn :: Parser Insn
insn = choice [nopInsnP,(try assignInsnP),arithInsn,jmpInsn,condJmpInsn]

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
  do whiteSpace
     is <- many1 stmt
     eof
     return is

parseNkmmAs :: String -> Either ParseError Program
parseNkmmAs = parse nkmm "(unknown)"

doParse :: String -> IO (Maybe Program)
doParse src = case (parseNkmmAs src) of
                  (Left err) -> do hPutStrLn stderr $ show err
                                   return Nothing
                  (Right prog) -> return $ Just prog

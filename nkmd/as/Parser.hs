module Parser (parseNkmmAs) where

import Insn

import Control.Monad
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

lexer :: P.TokenParser ()
lexer = P.makeTokenParser style
  where style = emptyDef {
    P.reservedOpNames = ["=", "+", "-", "|", "&", "^", "!", ";", "[", "]"],
    P.commentLine = "#" }
    -- P.reservedNames = ["R0", "A", "B", "C", "D", "E", "SP", "PC"],

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

regSel :: Parser RegSel
regSel = (reserved "c0" >> return Rc0)
       <|> (reserved "a" >> return Ra)
       <|> (reserved "b" >> return Rb)
       <|> (reserved "c" >> return Rc)
       <|> (reserved "d" >> return Rd)
       <|> (reserved "e" >> return Re)
       <|> (reserved "c" >> return Rf)
       <?> "register"

imm :: Parser Integer
imm = P.natural lexer

regImm :: Parser (Either RegSel Integer)
regImm = liftM Left regSel <|> liftM Right imm

loadExpr :: Parser AluExprT
loadExpr = do bsel <- regImm
              return $ AluExpr OpAdd Rc0 bsel
           <?> "loadExpr"

notExpr = do reservedOp "!"
             bsel <- regImm
             return $ AluExpr OpNot Rc0 bsel
           <?> "notExpr"


makeAluExpr :: (Parser ()) -> AluSel -> Parser AluExprT
makeAluExpr opP alu = do asel <- regSel
	                 opP
                         bsel <- regImm
                         return $ AluExpr alu asel bsel
                      <?> "aluExpr"

aluExpr :: Parser AluExprT
aluExpr = (try $ makeAluExpr (reservedOp "+") OpAdd)
        <|> (try $ makeAluExpr (reservedOp "-") OpSub)
        <|> (try $ makeAluExpr (reservedOp "|") OpOr)
        <|> (try $ makeAluExpr (reservedOp "&") OpAnd)
        <|> (try $ makeAluExpr (reservedOp "^") OpXor)
        <|> notExpr
        <|> loadExpr

-- mWriteInsn :: Parser Insn
-- mWriteInsn = do reservedOp "["
--                 alue <- aluExpr
-- 		reservedOp "]"
--                 reservedOp "<-"
--                 rd <- regSel
--                 reservedOp ";"
--                 return Insn { memw = True, memr = False, rd = rd, alue = alue }
--              <?> "mWriteInsn"
-- 
-- mrReadInsn :: Parser Insn
-- mrReadInsn = do rd <- regSel
--                 reservedOp "<-"
-- 		memr <- option False (reservedOp "[" >> return True)
--                 alue <- aluExpr
-- 		when memr (reservedOp "]")
--                 reservedOp ";"
--                 return Insn { memw = False, memr = memr, rd = rd, alue = alue }
--              <?> "mrReadInsn"

memSel :: Parser MemSel
memSel = (reserved "R" >> return MR)
       <|> (reserved "C" >> return MC)

memrLBracket :: Parser MemSel
memrLBracket = option MNone $
               do reservedOp "["
                  m <- memSel
		  return m

rAssignInsn :: Parser Insn
rAssignInsn = do rd <- regSel
                 reservedOp "="
		 memr <- memrLBracket
                 alue <- aluExpr
		 when (memr /= LEA) (reservedOp "]")
		 return Insn { memr = memr, memw = MNone, rd = rd, alue = alue }
		 
insn :: Parser Insn
insn = choice [rAssignInsn]
 
nkmm :: Parser Object
nkmm = do is <- many1 insn
          eof
          return is

parseNkmmAs :: String -> Either ParseError Object
parseNkmmAs = parse nkmm "(unknown)"

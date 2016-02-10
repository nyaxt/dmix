module Main(main) where

import Control.Monad
import Data.List
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

data RegSel = R0 | RegA | RegB | RegC | RegD | RegE | SP | PC 
instance Show RegSel where
  show R0 = "R0"
  show RegA = "A"
  show RegB = "B"
  show RegC = "C"
  show RegD = "D"
  show RegE = "E"
  show SP = "SP"
  show PC = "PC"

data AluSel = OpAdd | OpSub | OpOr | OpAnd | OpXor | OpNot | OpShift deriving Show

data AluExprT = AluExpr AluSel RegSel (Either RegSel Integer)
instance Show AluExprT where
  show (AluExpr alu asel (Left bsel)) = (show alu)++"("++(show asel)++", "++(show bsel)++")"
  show (AluExpr alu asel (Right imm)) = (show alu)++"("++(show asel)++", imm "++(show imm)++")"

data Insn =
  Insn {memw :: Bool,
        memr :: Bool,
        dsel :: RegSel,
        alue :: AluExprT }
instance Show Insn where
  show (Insn {memw = w, memr = r, dsel = d, alue = a}) =
    "Insn{M["++(if w then "W" else "-")++(if r then "R" else "-")++"] "++
    (show d)++" "++(show a)++"}"

-- instance Show Insn where
--   show (Insn {memw = False, memr = False, dsel = dsel, alue = alue}) =
--     show dsel++" <- "++show alue
--   show (Insn {memw = False, memr = True, dsel = dsel, alue = alue}) =
--     show dsel++" <- ["++show alue++"]"
--   show (Insn {memw = True, memr = False, dsel = dsel, alue = alue}) =
--     "["++show alue++"] <- "++show dsel
--   show (Insn {memw = True, memr = True, dsel = dsel, alue = alue}) =
--     "Insn {memw = True, memr = True}" -- Error!!

type Object = [Insn]

lexer :: P.TokenParser ()
lexer = P.makeTokenParser style
  where style = emptyDef {
    P.reservedOpNames = ["<-", "+", "-", "|", "&", "^", "!", ";", "[", "]"],
    P.reservedNames = ["R0", "A", "B", "C", "D", "E", "SP", "PC"],
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

loadExpr :: Parser AluExprT
loadExpr = do bsel <- regImm
              return $ AluExpr OpAdd R0 bsel
           <?> "loadExpr"

notExpr = do reservedOp "!"
             bsel <- regImm
             return $ AluExpr OpNot R0 bsel
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

mWriteInsn :: Parser Insn
mWriteInsn = do reservedOp "["
                alue <- aluExpr
		reservedOp "]"
                reservedOp "<-"
                dsel <- regSel
                reservedOp ";"
                return Insn { memw = True, memr = False, dsel = dsel, alue = alue }
             <?> "mWriteInsn"

mrReadInsn :: Parser Insn
mrReadInsn = do dsel <- regSel
                reservedOp "<-"
		memr <- option False (reservedOp "[" >> return True)
                alue <- aluExpr
		when memr (reservedOp "]")
                reservedOp ";"
                return Insn { memw = False, memr = memr, dsel = dsel, alue = alue }
             <?> "mrReadInsn"
            
insn :: Parser Insn
insn = choice [mWriteInsn, mrReadInsn]
 
nkmm :: Parser Object
nkmm = do is <- many1 insn
          eof
          return is

parseNkmmAs :: String -> Either ParseError Object
parseNkmmAs = parse nkmm "(unknown)"

showObj :: Object -> String
showObj obj = intercalate "\n" $ map show obj

main :: IO ()
main = do src <- getContents
	  case (parseNkmmAs src) of
	    (Left err) -> putStrLn $ show err
	    (Right obj) -> putStrLn $ showObj obj 

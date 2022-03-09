module RegParser (parseRegexpr, RegExpr(..)) where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)

mychar = Parsec.noneOf "*()|"

data RegExpr = Letter(Char)
             | AnyLetter
             | EmptyChar
             | Kleene(RegExpr)
             | Concat(RegExpr,RegExpr)
             | Union(RegExpr, RegExpr)
             | EndReg
             | NotEndReg
             deriving(Eq, Show)

parseChar :: Parsec.Parsec String () (RegExpr)
parseChar = do
       mylet <- mychar
       return $ case mylet of
              '.' -> (AnyLetter)
              '_' -> (EmptyChar)
              _ -> (Letter(mylet))

parseFactor :: Parsec.Parsec String () (RegExpr)
parseFactor = do
        mychar <- parseChar
        kleene <- Parsec.optionMaybe (Parsec.char '*')
        return $ case kleene of
               Just '*' -> (Kleene(mychar))
               _ -> (mychar)

parseEnd1 :: Parsec.Parsec String () (RegExpr)
parseEnd1 = do
        feof <- Parsec.lookAhead (Parsec.eof)
        return EndReg

parseEnd2 :: Parsec.Parsec String () (RegExpr)
parseEnd2 = do
        Parsec.lookAhead (Parsec.oneOf "|)")
        return EndReg

parseEnd :: Parsec.Parsec String () (RegExpr)
parseEnd = do
        end1 <- Parsec.try (parseEnd1 <|> parseEnd2)
        return end1

pParen :: Parsec.Parsec String () (RegExpr)
pParen = do
        Parsec.char '('
        word <- pStmt4
        (Parsec.char ')')
        return (word)

pParenOrKleene :: Parsec.Parsec String () (RegExpr)
pParenOrKleene = do
        par <- pParen
        kleene <- Parsec.optionMaybe (Parsec.char '*')
        return $ case kleene of
               Just '*' -> Kleene(par)
               _ -> (par)

pStmt :: Parsec.Parsec String () (RegExpr)
pStmt = do
        stmt <- Parsec.try (pParenOrKleene <|> parseFactor)
        return stmt

pStmt2 :: Parsec.Parsec String () (RegExpr)
pStmt2 = do
        stmt <- pStmt
        rest <- pStmt3
        return $ case rest of
                EndReg -> (stmt)
                _ -> (Concat(stmt, rest))

pStmt2wrap :: Parsec.Parsec String () (RegExpr)
pStmt2wrap = do
        stmt <- pStmt2
        Parsec.notFollowedBy (Parsec.char '|')
        return stmt

pStmt3 :: Parsec.Parsec String () (RegExpr)
pStmt3 = do
        stmt <- (Parsec.try (parseEnd <|> pStmt2))
        return stmt

pStmt4 :: Parsec.Parsec String () (RegExpr)
pStmt4 = do
        stmt <- (Parsec.choice [Parsec.try pStmt2wrap, Parsec.try pStmt5])
        return stmt

pStmt5 :: Parsec.Parsec String () (RegExpr)
pStmt5 = do
        stmt <- pStmt2
        Parsec.char '|'
        stmt2 <- pStmt2
        return (Union(stmt,stmt2))

parseRegexpr mystr = a
  where {
  Right a = Parsec.parse pStmt4 "__" mystr;
}

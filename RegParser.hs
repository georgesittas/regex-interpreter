module RegParser (parseRegexpr, RegExpr(..)) where

import Text.Parsec ( char, noneOf, choice, parse, eof )
import Text.Parsec.Char ()
import Control.Applicative ((<**>))
import qualified Text.Parsec.Prim as Prim
import Text.Parsec.String ( Parser )

data RegExpr = Letter Char
             | AnyLetter
             | EmptyChar
             | Kleene RegExpr
             | Concat RegExpr RegExpr
             | Union  RegExpr RegExpr
             deriving(Eq, Show)
 
castChar :: Char -> RegExpr
castChar '.' = AnyLetter
castChar '_' = EmptyChar
castChar x   = Letter x 

charToken :: Parser RegExpr
charToken = castChar <$> noneOf "()*|"

parens :: Parser RegExpr
parens = char '(' *> regexParser <* char ')'

term :: Parser RegExpr
term = charToken Prim.<|> parens

kleeneLookAhead :: Parser (RegExpr -> RegExpr)
kleeneLookAhead = choice [Kleene <$ char '*', return id]

kleeneTerm :: Parser RegExpr
kleeneTerm = (charToken Prim.<|> parens) <**> kleeneLookAhead 

concatTerm :: Parser RegExpr
concatTerm = kleeneTerm <**> choice [flip Concat <$> concatTerm, return id]

regexParser :: Parser RegExpr
regexParser = concatTerm <**> choice [flip Union <$> (char '|' *> regexParser) , return id]

parseRegexpr :: String -> RegExpr
parseRegexpr str = let Right a = parse (regexParser <* eof) "" str in a 

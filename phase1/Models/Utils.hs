module Models.Utils where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

parseInit :: String -> Parser ()
parseInit s = do
    string s
    skipSome (char '{' <|> char ' ')

parseSep :: Char -> Parser ()
parseSep c = skipMany (char c <|> char ' ')

parseInt :: String -> Parser Int
parseInt s = do
  string s
  parseSep '='
  read <$> some digitChar

parseString :: String -> Parser String
parseString s = do
  string s
  parseSep '='
  between (char '"') (char '"') (some alphaNumChar)

parseBool :: String -> Parser Bool
parseBool s = do
    string s
    parseSep '='
    read <$> (string "True" <|> string "False")

parseSomethingWithRead :: Read a => String -> Parser a
parseSomethingWithRead s = do
    string s
    parseSep '='
    read <$> someTill asciiChar (char ',')

withRemaining :: Parser a -> Parser (a, String)
withRemaining p = (,) <$> p <*> getInput

singl :: a -> [a]
singl a = [a]

split :: (a -> b) -> (a -> c) -> a -> (b, c)
split f g a = (f a, g a)

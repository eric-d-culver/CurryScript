module Main (main) where

import ParserType as P
import Control.Applicative(Alternative((<|>), empty, some, many))

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

vowelParser :: [Char] -> [(Char, [Char])]
vowelParser [] = []
vowelParser (x:xs) = case isVowel x of
        True -> [(x, xs)]
        False -> []

vParser = P.Parser vowelParser

twoVowelParser :: P.Parser Char String
twoVowelParser  = do
                 fir <- vParser
                 sec <- vParser
                 return [fir, sec]

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

digitParser :: [Char] -> [(Int, [Char])]
digitParser [] = []
digitParser (x:xs) = case isDigit x of
                True -> [(read [x], xs)]
                False -> []

dParser = P.Parser digitParser

vowels :: Int -> P.Parser Char String
vowels 0 = do
            return []
vowels n = do
            fir <- vParser
            res <- vowels (n-1)
            return (fir:res)

vowelsParser :: P.Parser Char String
vowelsParser = do
                digit <- dParser
                vowels digit

test = do
        putStrLn $ show $ runParser twoVowelParser "eieio"
        putStrLn $ show $ runParser vowelsParser "3eieio"
        putStrLn $ show $ runParser (twoVowelParser <|> vowelsParser) "3eieio"
        putStrLn $ show $ runParser (many vParser) "eieiohu"
        putStrLn $ show $ runParser (many vParser) "hello"
        putStrLn $ show $ runParser (some vParser) "eieiohu"
        putStrLn $ show $ runParser (some vParser) "hello"

main = test

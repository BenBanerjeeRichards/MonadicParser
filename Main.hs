module Main where

import Data.Char
import Control.Applicative

main :: IO ()
main = putStrLn "Hello, Haskell!"

newtype Parser a = Parser {
    parse :: String -> [(a, String)]
}

-- For ghci testing
p1 = parseChar 'a' 
p2 = parseChar 'b'

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p1 = Parser $ \input ->
        [ (f match, remaining) | (match, remaining) <- parse p1 input]

instance Applicative Parser where
    pure x = result x
    -- <*> Parser (a -> b) -> Parser a -> Parser b
    (<*>) fparser p  = 
        Parser $ \input -> [x | (f, remaining) <- parse fparser input, 
                                       x <- parse (fmap f p) remaining]  
instance Monad Parser where
    return = result

    (>>=) = bind
    (>>) p1 p2 = Parser $ \input -> concat [parse p2 remaining | 
                                    (match, remaining) <- parse p1 input]

instance Alternative Parser where
    empty = zero

    (<|>) p1 p2 = Parser $ \input -> 
        let attempt1 = parse p1 input
            attempt2 = parse p2 input
        in attempt1 ++ attempt2 

parseChar :: Char -> Parser Char
parseChar c =
    Parser parse

    where
        parse :: String -> [(Char, String)]
        parse [] = []
        parse (x:xs)
            | x == c = [(x, xs)]
            | otherwise = []

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \input ->
            let matches = parse p input 
            in concat [parse (f val) remaining | (val, remaining) <- matches]

-- Identity
result :: a -> Parser a
result v = Parser $ \input -> [(v, input)]

item :: Parser Char
item = Parser $ \input ->
    case input of
        [] -> []
        (x:xs) -> [(x, xs)]

zero :: Parser a
zero = Parser $ \input -> []

sat :: (Char -> Bool) -> Parser Char
sat predicate =
    bind item (\x -> if predicate x
                      then result x
                      else zero)

-- Two lower case strings, returns string of length 2 ("abcd" -> "ab")
lower :: Parser Char
lower = sat isLower 

lower2 :: Parser String
lower2 = do
    x <- lower 
    y <- lower 
    return [x, y]
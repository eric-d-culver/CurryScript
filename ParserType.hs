
module ParserType (Parser(..))  where

import Control.Applicative(Alternative((<|>), empty, many, some))
import Control.Monad(MonadPlus(mzero, mplus))

data Parser t v = Parser {
                         runParser :: [t] -> [(v, [t])]
                         }
-- A Parser takes a stream of tokens of type t and parses a value of type v
-- it also returns the remaining unparsed stream
-- it returns a list to allow the possibility of multiple parses or none

instance Monad (Parser t) where
        (Parser f) >>= g = Parser $ \stream -> [(v2, rest2) | (v1, rest1) <- (f stream), (v2, rest2) <- (runParser (g v1) rest1)]
        return x = Parser $ \stream -> [(x,stream)]

instance Applicative (Parser t) where
        pure = return
        m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))

instance Functor (Parser t) where
        fmap f xs = xs >>= return . f

instance Alternative (Parser t) where
        empty = Parser $ \stream -> []
        (Parser f) <|> (Parser g) = Parser $ \stream -> (f stream) ++ (g stream)
	-- many :: Parser t a -> Parser t [a]
	-- some :: Parser t a -> Parser t [a]

instance MonadPlus (Parser t)

-- Healper functions for creating parsers

satisfy :: (t -> Bool) -> Parser t ()
-- satisfy f = 

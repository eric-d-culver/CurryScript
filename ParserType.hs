
module Parser.Type (Parser(..))  where

data Parser v t = Parser ([t] -> [(v, [t])])
-- A Parser takes a stream of tokens of type t and parses a value of type v
-- it also returns the remaining unparsed stream
-- it returns a list to allow the possibility of multiple parses or none

module Entrypoint where

import Text.Parsec


type Toplevel = [Statement] 
data Statement =
    TypeDef String TypeVal 
    | TermDef String TermVal

data Modality = At | Box
    deriving stock (Eq, Show)

data TypeVal = Fun TypeVal TypeVal | Prod TypeVal TypeVal | Modal Modality TypeVal | Sum TypeVal TypeVal | List TypeVal | Unit
    deriving stock (Eq, Show)
data TermVal = Var String | Lam | App | Fst | Snd | MkProd | Left' | Right' | Either | Nil | Cons | ListRec | TT
    deriving stock (Eq, Show)

-- Implementation of infix operators with binding strength and fixity by
-- first parsing into type lists
-- data TypeOp = FunOp | ProdOp | SumOp
-- data TypeList = TNil | TCons 



stringParser:: Parsec String st String
stringParser = many anyChar

white :: Parsec String st ()
white = spaces

basetypeParser:: Parsec String st TypeVal
basetypeParser = string "Unit" *> return Unit
    <|> between (string "(") (string ")") typeParser

typeParser:: Parsec String st TypeVal
typeParser = 
    basetypeParser <* spaces >>= \x -> choice
     [ eof >> return x
     , lookAhead (string ")") >> return x
     , string "->" >> spaces >> typeParser >>= \y -> return (Fun x y)
     , string "," >> spaces >> typeParser >>= \y -> return (Prod x y)
     ]

    -- Fun <$> typeParser <* spaces <* string "->" <* spaces <*> typeParser
    -- <|> (string "Unit" *> return Unit)


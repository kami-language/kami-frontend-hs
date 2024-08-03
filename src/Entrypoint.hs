module Entrypoint where

import Text.Parsec
import Control.Monad


type Toplevel = [Statement]
data Statement =
    TypeDef Name TypeVal
    | TermDef Name TermVal
    deriving stock (Eq, Show)

data Modality = At | Box
    deriving stock (Eq, Show)

data TypeVal = Fun TypeVal TypeVal | Prod TypeVal TypeVal | Modal Modality TypeVal | Sum TypeVal TypeVal | List TypeVal | Unit
    deriving stock (Eq, Show)
data TermVal =
    Var String
    | Lam [FunArg] TermVal | App TermVal TermVal
    | Fst TermVal | Snd TermVal | MkProd TermVal TermVal
    | Left' TermVal | Right' TermVal | Either' TermVal TermVal TermVal
    | Nil | Cons | ListRec
    | TT
    deriving stock (Eq, Show)

-- Implementation of infix operators with binding strength and fixity by
-- first parsing into type lists
-- data TypeOp = FunOp | ProdOp | SumOp
-- data TypeList = TNil | TCons 

newtype Name = Name {getName :: String}
    deriving newtype (Eq, Show)

data FunArg = FunArg Name TypeVal
    deriving stock (Eq, Show)

stringParser:: Parsec String st String
stringParser = many anyChar

white :: Parsec String st ()
white = spaces

spaces' :: Parsec String st ()
spaces' = space >> spaces

nameParser :: Parsec String st Name
nameParser = Name <$> many1 alphaNum

basetypeParser:: Parsec String st TypeVal
basetypeParser = string "Unit" *> return Unit
    <|> between (string "(") (string ")") typeParser

typeParser:: Parsec String st TypeVal
typeParser =
    basetypeParser <* spaces >>= \x -> choice
     [ string "->" >> spaces >> typeParser >>= \y -> return (Fun x y)
     , string "," >> spaces >> typeParser >>= \y -> return (Prod x y)
     , return x
     ]

argParser :: Parsec String st FunArg
argParser = between (string "(") (string ")")
    (FunArg <$> nameParser <* spaces <* string ":" <* spaces <*> typeParser)

basetermParser:: Parsec String st TermVal
basetermParser = choice
    [ string "tt" >> return TT
    , string "\\" >> Lam <$> many1 (argParser <* spaces) <* string "->" <* spaces <*> termParser'
    , try $ between (string "(") (string ")") termParser'


    , string "fst" >> spaces' >> Fst <$> basetermParser
    , string "snd" >> spaces' >> Snd <$> basetermParser

    , string "left" >> spaces' >> Left' <$> basetermParser
    , string "right" >> spaces' >> Right' <$> basetermParser
    , string "either" >> spaces' >> Either' <$> basetermParser <* spaces' <*> basetermParser <* spaces' <*> basetermParser
    ]


appParser':: TermVal -> Parsec String st TermVal
appParser' acc' = f acc' <$> many (try (spaces' *> basetermParser))
    where
        f acc [] = acc
        f acc (x : xs) = f (App acc x) xs


termParser':: Parsec String st TermVal
termParser' = basetermParser >>= \x -> choice
    [ try $ MkProd x <$ spaces <* string "," <* spaces <*> termParser'
    , try $ appParser' x
    ]


termParser:: Parsec String st TermVal
termParser =  termParser' <* eof


statementParser :: Parsec String st Statement
statementParser = TypeDef <$> nameParser <* spaces <* string ":"  <* spaces <*> typeParser
    <|> TermDef <$> nameParser <* spaces <* string "="  <* spaces <*> termParser'


    -- Fun <$> typeParser <* spaces <* string "->" <* spaces <*> typeParser
    -- <|> (string "Unit" *> return Unit)


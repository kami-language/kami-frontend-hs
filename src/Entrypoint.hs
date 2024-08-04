module Entrypoint where

import Text.Parsec
import Control.Monad
import Data.Filtrable ( Filtrable(mapMaybe) )


type Toplevel = [Statement]
data Statement =
    TypeDef Name TypeVal
    | TermDef Name [Name] TermVal
    deriving stock (Eq, Show)

data Modality = At | Box
    deriving stock (Eq, Show)

data TypeVal = Fun TypeVal TypeVal | Prod TypeVal TypeVal | Modal Modality TypeVal | Sum TypeVal TypeVal | List TypeVal | Unit
    deriving stock (Eq, Show)
data TermVal =
    Var Name
    | Lam FunArg TermVal | App TermVal TermVal
    | LetIn FunArg TermVal TermVal
    | Fst TermVal | Snd TermVal | MkProd TermVal TermVal
    | Left' TermVal | Right' TermVal | Either' TermVal TermVal TermVal
    | Nil | Cons | ListRec
    | TT
    | Check TermVal TypeVal
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
white = many (char ' ') *> return ()

white' :: Parsec String st ()
white' = many1 (char ' ') *> return ()

nameParser :: Parsec String st Name
nameParser = Name <$> many1 alphaNum

basetypeParser:: Parsec String st TypeVal
basetypeParser = string "Unit" *> return Unit
    <|> between (string "(") (string ")") typeParser

typeParser:: Parsec String st TypeVal
typeParser =
    basetypeParser <* white >>= \x -> choice
     [ string "->" >> white >> typeParser >>= \y -> return (Fun x y)
     , string "," >> white >> typeParser >>= \y -> return (Prod x y)
     , return x
    --  , lookAhead (string ")") >> return x
     ]

argParser :: Parsec String st FunArg
argParser = between (string "(") (string ")")
    (FunArg <$> nameParser <* white <* string ":" <* white <*> typeParser)

mkLam ::  [FunArg] -> TermVal -> TermVal
mkLam xs t = foldr Lam t xs

basetermParser:: Parsec String st TermVal
basetermParser = choice
    [ string "tt" >> return TT
    , string "\\" >> mkLam <$> many1 (argParser <* white) <* string "->" <* white <*> termParser'
    , try $ between (string "(") (string ")") termParser'

    , string "fst" >> white' >> Fst <$> basetermParser
    , string "snd" >> white' >> Snd <$> basetermParser

    , string "left" >> white' >> Left' <$> basetermParser
    , string "right" >> white' >> Right' <$> basetermParser
    , string "either" >> white' >> Either' <$> basetermParser <* white' <*> basetermParser <* white' <*> basetermParser

    , Var <$> nameParser
    ]


appParser':: TermVal -> Parsec String st TermVal
appParser' acc' = f acc' <$> many (try (white' *> basetermParser))
    where
        f acc [] = acc
        f acc (x : xs) = f (App acc x) xs


termParser':: Parsec String st TermVal
termParser' = basetermParser >>= \x -> choice
    [ try $ MkProd x <$ white <* string "," <* white <*> termParser'
    , try $ appParser' x
    ]


termParser:: Parsec String st TermVal
termParser =  termParser' <* eof


statementParser :: Parsec String st Statement
statementParser = choice
    [ try $ TypeDef <$> nameParser <* white <* string ":"  <* white <*> typeParser
    , TermDef <$> nameParser <* white' <*> many (nameParser <* white') <* string "="  <* white' <*> termParser'
    ]

statementsParser :: Parsec String st [Statement]
statementsParser = (statementParser `sepBy` (white <* endOfLine)) <* eof

-- Parsing a whole file means first parsing
-- into statements, and then combining all
-- statements into a single term (using let-in).
statementsIntoTerm :: [Statement] -> Either String TermVal
statementsIntoTerm statements = do

    -- split term and type definitions
    let fType (TypeDef name d) = Just (name , d)
        fType (TermDef _ _ _) = Nothing
    let fTerm (TermDef name args d) = Just (name , args , d)
        fTerm (TypeDef _ _) = Nothing
    let terms = mapMaybe fTerm statements
    let types = mapMaybe fType statements

    -- apply type to the arguments of a term
    let applyType [] ty t = Right $ Check t ty
        applyType (x : xs) (Fun a b) t = do
            t' <- applyType xs b t
            return (Lam (FunArg x a) t')
        applyType (_ : _) ty _ = Left $ "Expected the type " <> show ty <> " to be a function type"

    -- find type for every term and turn function definition into lambdas
    let intoLambda (name , args , t) = do
            let mytypes = filter (\(typename, _) -> typename == name) types
            case mytypes of
                [] -> Left $ "no type for term " <> show name
                [(_, mytype)] -> do
                    newterm <- applyType args mytype t
                    return (name, newterm , mytype)
                _ -> Left $ "There are multiple types for term " <> show name

    -- the list of function definitions changed into lambda terms
    terms' <- mapM intoLambda terms

    -- we only have to combine all of them into a large let-in
    let intoLetIn [(_, val, ty)] = Check val ty
        intoLetIn ((name, val, ty):xs) = LetIn (FunArg name ty) val (intoLetIn xs)
        intoLetIn [] = TT

    return $ intoLetIn terms'



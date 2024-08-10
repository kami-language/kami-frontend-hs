
module Parser.Definition where

import Text.Parsec
import Control.Monad
import Data.Filtrable ( Filtrable(mapMaybe) )
import Data.Text (Text)
import qualified Data.Text as T

newtype Name = Name {getName :: Text}
    deriving stock (Eq, Show)

data Location = L0 | L1 | L2
    deriving stock (Eq, Show)

data Modality = At Location | Box
    deriving stock (Eq, Show)

data TypeVal = Fun TypeVal TypeVal | Prod TypeVal TypeVal | Modal Modality TypeVal | Sum TypeVal TypeVal | List TypeVal | Unit
    deriving stock (Eq, Show)

data FunArg = TypeFunArg Name TypeVal | NameFunArg Name
    deriving stock (Eq, Show)

data TermVal =
    Var Name
    | Lam FunArg TermVal | App TermVal TermVal
    | LetIn FunArg TermVal TermVal
    | Mod Modality TermVal
    | Fst TermVal | Snd TermVal | MkProd TermVal TermVal
    | Left' TermVal | Right' TermVal | Either' TermVal TermVal TermVal
    | Nil | Cons TermVal TermVal | ListRec TermVal TermVal TermVal
    | TT
    | Check TermVal TypeVal
    deriving stock (Eq, Show)


data Statement =
    TypeDef Name TypeVal
    | TermDef Name [Name] TermVal
    | EmptyLine
    | Comment String
    deriving stock (Eq, Show)

type Toplevel = [Statement]


-- Implementation of infix operators with binding strength and fixity by
-- first parsing into type lists
-- data TypeOp = FunOp | ProdOp | SumOp
-- data TypeList = TNil | TCons 



stringParser:: Parsec String st String
stringParser = many anyChar

white :: Parsec String st ()
white = many (char ' ') *> return ()

white' :: Parsec String st ()
white' = many1 (char ' ') *> return ()

nameParser :: Parsec String st Name
nameParser = Name <$> T.pack <$> many1 alphaNum

basetypeParser:: Parsec String st TypeVal
basetypeParser = string "Unit" *> return Unit
    <|> string "List" *> white' *> (List <$> basetypeParser)
    <|> between (string "(") (string ")") typeParser
    <|> between (string "{") (string "}") (Modal Box <$> typeParser)

locationParser :: Parsec String st Location
locationParser = choice
    [ string "0" >> return L0
    , string "1" >> return L1
    , string "2" >> return L2
    ]

typeParser:: Parsec String st TypeVal
typeParser =
    basetypeParser <* white >>= \x -> choice
     [ string "->" >> white >> typeParser >>= \y -> return (Fun x y)
     , string "," >> white >> typeParser >>= \y -> return (Prod x y)
     , string "+" >> white >> typeParser >>= \y -> return (Sum x y)
     , string "@" >> white >> locationParser >>= \y -> return (Modal (At y) x)
     , return x
    --  , lookAhead (string ")") >> return x
     ]

argParser :: Parsec String st FunArg
argParser =  choice
    [ between (string "(") (string ")") (TypeFunArg <$> nameParser <* white <* string ":" <* white <*> typeParser)
    , NameFunArg <$> nameParser
    ]

mkLam ::  [FunArg] -> TermVal -> TermVal
mkLam xs t = foldr Lam t xs

basetermParser:: Parsec String st TermVal
basetermParser = choice
    [ string "\\" >> mkLam <$> many1 (argParser <* white) <* string "->" <* white <*> termParser'
    , try $ string "tt" >> return TT
    , try $ between (string "(") (string ")") termParser'
    , try $ between (string "{") (string "}") (Mod Box <$> termParser') -- mod for box modality
    , try $ string "let" >> white' >> LetIn <$> argParser <* white' <* string "=" <* white' <*> termParser' <* spaces <* string "in" <* white' <*> termParser'

    , try $ string "fst" >> white' >> Fst <$> basetermParser
    , try $ string "snd" >> white' >> Snd <$> basetermParser

    , try $ string "left" >> white' >> Left' <$> basetermParser
    , try $ string "right" >> white' >> Right' <$> basetermParser
    , try $ string "either" >> white' >> Either' <$> basetermParser <* white' <*> basetermParser <* white' <*> basetermParser

    , try $ string "nil" >> return Nil
    , try $ string "cons" >> white' >> Cons <$> basetermParser <* white' <*> basetermParser
    , try $ string "rec-List" >> white' >> ListRec <$> basetermParser <* white' <*> basetermParser <* white' <*> basetermParser

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
    , try $ (flip Mod x . At) <$ white <* string "@" <* white <*> locationParser -- mod for at modality
    , try $ appParser' x
    ]


termParser:: Parsec String st TermVal
termParser =  termParser' <* eof


statementParser :: Parsec String st Statement
statementParser = choice
    [ Comment <$> string "//" <* many (noneOf "\n")
    , try $ TypeDef <$> nameParser <* white <* string ":"  <* white <*> typeParser
    , TermDef <$> nameParser <* white' <*> many (nameParser <* white') <* string "="  <* white' <*> termParser'
    , lookAhead endOfLine *> return EmptyLine
    , lookAhead eof *> return EmptyLine
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
        fType (TermDef {}) = Nothing
        fType (Comment _ ) = Nothing
        fType EmptyLine = Nothing
    let fTerm (TermDef name args d) = Just (name , args , d)
        fTerm (TypeDef _ _) = Nothing
        fTerm (Comment _ ) = Nothing
        fTerm EmptyLine = Nothing
    let terms = mapMaybe fTerm statements
    let types = mapMaybe fType statements

    -- apply type to the arguments of a term
    let applyType [] ty t = Right t -- Check t ty
        applyType (x : xs) (Fun a b) t = do
            t' <- applyType xs b t
            return (Lam (TypeFunArg x a) t')
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
        intoLetIn ((name, val, ty):xs) = LetIn (TypeFunArg name ty) val (intoLetIn xs)
        intoLetIn [] = TT

    return $ intoLetIn terms'




module ParserSpec where

import Parser.Definition
import Test.Hspec -- (describe , Spec)

import Text.Parsec ( parse )

test p = parse p ""

spec :: Spec
spec = do
    describe "typeParser" $ do
        it "can parse Arrow types" $ do
            test typeParser "Unit" `shouldBe` Right Unit
            test typeParser "Unit -> Unit" `shouldBe` Right (Fun Unit Unit)
            test typeParser "Unit -> Unit -> Unit" `shouldBe` Right (Fun Unit (Fun Unit Unit))

        it "can parse Product types" $ do
            test typeParser "Unit , Unit" `shouldBe` Right (Prod Unit Unit)
            test typeParser "Unit , Unit , Unit" `shouldBe` Right (Prod Unit (Prod Unit Unit))

        it "can parse Parenthesis in types" $ do
            test typeParser "Unit -> Unit , Unit" `shouldBe` Right (Fun Unit (Prod Unit Unit))
            test typeParser "(Unit -> Unit) , Unit" `shouldBe` Right (Prod (Fun Unit Unit) Unit)

        it "can parse types with modalities" $ do
            test typeParser "(List Unit @ 0) -> {List (Unit @ 0)} @ 0" `shouldBe`
                Right (Fun (Modal (At L0) (List Unit)) (Modal (At L0) (Modal Box (List (Modal (At L0) Unit)))))

    describe "termParser" $ do
        it "can parse tt" $ do
            test termParser "tt" `shouldBe` Right TT

        it "can parse lambdas" $ do
            test termParser "\\(f : Unit -> Unit) -> tt" `shouldBe` Right (mkLam [TypeFunArg (Name "f") (Fun Unit Unit)] TT)
            test termParser "\\(f : Unit -> Unit) (a : Unit) -> tt" `shouldBe` Right (mkLam [TypeFunArg (Name "f") (Fun Unit Unit), TypeFunArg (Name "a") Unit] TT)

        it "can parse function applications" $ do
            test termParser "tt tt" `shouldBe` Right (App TT TT)
            test termParser "tt (tt tt tt)" `shouldBe` Right (App TT (App (App TT TT) TT))
            test termParser "(fst tt) (snd tt)" `shouldBe` Right
                (App (Fst TT) (Snd TT))

        it "can parse product terms" $ do
            test termParser "fst tt" `shouldBe` Right (Fst TT)
            test termParser "tt , tt" `shouldBe` Right (MkProd TT TT)
            test termParser "(tt , tt)" `shouldBe` Right (MkProd TT TT)
            test termParser "snd (tt , tt)" `shouldBe` Right (Snd (MkProd TT TT))
            test termParser "(fst tt) (snd tt , tt)" `shouldBe` Right
                (App (Fst TT) (MkProd (Snd TT) TT))
            test termParser "(tt , (fst tt) (snd tt , tt))" `shouldBe` Right
                (MkProd TT (App (Fst TT) (MkProd (Snd TT) TT)))
            test termParser "tt , tt , tt" `shouldBe` Right
                (MkProd TT (MkProd TT TT))
            test termParser "fst tt tt" `shouldBe` Right
                (App (Fst TT) TT)

        it "can parse sum terms" $ do
            test termParser "left tt" `shouldBe` Right (Left' TT)
            test termParser "right tt" `shouldBe` Right (Right' TT)
            test termParser "either (left tt) tt tt" `shouldBe` Right (Either' (Left' TT) TT TT)

        it "can parse variables" $ do
            test termParser "\\(a : Unit) (b : Unit) -> a , b" `shouldBe` Right
                (mkLam [TypeFunArg (Name "a") Unit , TypeFunArg (Name "b") Unit] (MkProd (Var (Name "a")) (Var (Name "b"))))


    describe "statementParser" $ do
        it "can parse typedefs" $ do
            test statementParser "hello : Unit -> Unit" `shouldBe`
                Right (TypeDef (Name "hello") (Fun Unit Unit))

        it "can parse termdefs" $ do
            test statementsParser "id a = a" `shouldBe`
                Right [TermDef (Name "id") [Name "a"] (Var (Name "a"))]

    describe "statementsParser" $ do
        it "can parse id" $ do
            test statementsParser "id : Unit -> Unit\nid a = a" `shouldBe`
                Right [TypeDef (Name "id") (Fun Unit Unit), TermDef (Name "id") [Name "a"] (Var (Name "a"))]

    describe "full file parser" $ do
        it "can parse id" $ do
            content <- readFile "test/examples/id.kami"
            (either (Left . show) (statementsIntoTerm) (test statementsParser content)) `shouldBe` 
                Right (Check (Lam (TypeFunArg (Name "a") (Modal (At L0) Unit)) (Mod (At L0) (Var (Name "a"))))
                 (Fun (Modal (At L0) Unit) (Modal (At L0) Unit)))

        it "can parse globalize-list" $ do
            content <- readFile "test/examples/globalize-list.kami"
            (either (Left . show) (statementsIntoTerm) (test statementsParser content)) `shouldBe` 
                Right (Check (Lam (TypeFunArg (Name {getName = "xs"}) (Modal (At L0) (List Unit))) (ListRec (Var (Name {getName = "xs"})) Nil (Lam (TypeFunArg (Name {getName = "x"}) Unit) (Lam (TypeFunArg (Name {getName = "xs"}) (List (Modal (At L0) Unit))) (Cons (Var (Name {getName = "x"})) (Var (Name {getName = "xs"}))))))) (Fun (Modal (At L0) (List Unit)) (Modal (At L0) (Modal Box (List (Modal (At L0) Unit))))))

                -- Right (Check (Lam (FunArg (Name "xs") (Modal (At L0) (List Unit))) (ListRec (Var (Name "xs")) (Var "Nil") (Lam (FunArg "x" Unit) (Lam (FunArg "xs" (List (Modal (At L0) Unit))) (App (App (Var "Cons") (Var "x")) (Var "xs")))))) (Fun (Modal (At L0) (List Unit)) (Modal (At L0) (Modal Box (List (Modal (At L0) Unit)))))) 
                




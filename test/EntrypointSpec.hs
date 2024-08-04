module EntrypointSpec where

import Entrypoint
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

    describe "termParser" $ do
        it "can parse tt" $ do
            test termParser "tt" `shouldBe` Right TT

        it "can parse lambdas" $ do
            test termParser "\\(f : Unit -> Unit) -> tt" `shouldBe` Right (mkLam [FunArg (Name "f") (Fun Unit Unit)] TT)
            test termParser "\\(f : Unit -> Unit) (a : Unit) -> tt" `shouldBe` Right (mkLam [FunArg (Name "f") (Fun Unit Unit), FunArg (Name "a") Unit] TT)

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
                (mkLam [FunArg (Name "a") Unit , FunArg (Name "b") Unit] (MkProd (Var (Name "a")) (Var (Name "b"))))


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


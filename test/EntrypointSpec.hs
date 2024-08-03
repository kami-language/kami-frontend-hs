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
            test termParser "\\(f : Unit -> Unit) -> tt" `shouldBe` Right (Lam [FunArg (Name "f") (Fun Unit Unit)] TT)
            test termParser "\\(f : Unit -> Unit) (a : Unit) -> tt" `shouldBe` Right (Lam [FunArg (Name "f") (Fun Unit Unit), FunArg (Name "a") Unit] TT)

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


    describe "statementParser" $ do
        it "can parse typedefs" $ do
            test statementParser "hello : Unit -> Unit" `shouldBe`
                Right (TypeDef (Name "hello") (Fun Unit Unit))


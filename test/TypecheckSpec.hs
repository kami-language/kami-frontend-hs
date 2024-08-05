
module TypecheckSpec where

import Test.Hspec -- (describe , Spec)
import MAlonzo.Code.KamiCore.Pipeline.Main ( sayhello , isLambda )
import Parser.Definition


spec :: Spec
spec = do
    describe "Agda backend" $ do
        it "is integrated" $ do
            sayhello "bla" `shouldBe` "hello, bla"
        it "can accept terms" $ do
            isLambda (Lam (FunArg (Name "bla") Unit) TT) `shouldBe` "Lambda!"



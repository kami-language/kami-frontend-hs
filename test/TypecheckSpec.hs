
module TypecheckSpec where

import Test.Hspec -- (describe , Spec)
import MAlonzo.Code.KamiCore.Pipeline.Main ( sayhello , isLambda , approximateTypecheck )
import Parser.Definition


spec :: Spec
spec = do
    describe "Agda backend" $ do
        it "is integrated" $ do
            sayhello "bla" `shouldBe` "hello, bla"
        it "can accept terms" $ do
            approximateTypecheck (Lam (FunArg (Name "bla") Unit) (App (Var (Name "bla")) TT)) `shouldBe` "done"



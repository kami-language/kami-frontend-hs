
module TypecheckSpec where

import Test.Hspec -- (describe , Spec)
import MAlonzo.Code.KamiCore.Pipeline.Main ( sayhello , isLambda , approximateTypecheck )
import Parser.Definition
import Control.Monad

import Text.Parsec ( parse )
test p = parse p ""

spec :: Spec
spec = do
    describe "Agda backend" $ do
        it "is integrated" $ do
            sayhello "bla" `shouldBe` "hello, bla"
        it "can check app terms" $ do
            approximateTypecheck (Lam (TypeFunArg (Name "bla") Unit) (App (Var (Name "bla")) TT)) `shouldBe` "Expected function type on left side of application"

        it "can check example file (id)" $ do
            content <- readFile "test/examples/id.kami"
            (either (Left . show) (fmap approximateTypecheck . statementsIntoTerm) (test statementsParser content)) `shouldBe` 
                Right "done"

        it "can check example file (globalize-sum)" $ do
            content <- readFile "test/examples/globalize-sum.kami"
            (either (Left . show) (fmap approximateTypecheck . statementsIntoTerm) (test statementsParser content)) `shouldBe` 
                Right "done"

        it "can check example file (globalize-List)" $ do
            content <- readFile "test/examples/globalize-list.kami"
            (either (Left . show) (fmap approximateTypecheck . statementsIntoTerm) (test statementsParser content)) `shouldBe` 
                Right "done"



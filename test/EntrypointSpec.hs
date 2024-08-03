module EntrypointSpec where

import Entrypoint
import Test.Hspec -- (describe , Spec)

import Text.Parsec ( parse )

test p = parse p ""

spec :: Spec
spec = describe "main" $ do
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


module TypecheckSpec where

import Test.Hspec -- (describe , Spec)
import MAlonzo.Code.KamiCore.Pipeline.Main ( sayhello )


spec :: Spec
spec = do
    describe "Agda backend" $ do
        it "is integrated" $ do
            sayhello "bla" `shouldBe` "hello, bla"
    

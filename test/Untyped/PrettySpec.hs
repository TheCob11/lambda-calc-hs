module Untyped.PrettySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Untyped
import Data.List (isPrefixOf)

spec :: Spec
spec = do
    describe "Pretty printer :)" $ do
        it "Pretty's Var as itself" $ property $
            \s -> pretty (Var s) `shouldBe` s
        
        it "Pretty's Lambdas with \\x. y" $ do
            \s e -> pretty (Lam s e) `shouldSatisfy` ("\\x. " `isPrefixOf`)

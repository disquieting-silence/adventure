module ExampleSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "icebreaker" $ do
        prop "icebreaker" $ \x y ->
            add x y == add x y

add :: Int -> Int -> Int
add x y = x + y

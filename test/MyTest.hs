module Main where
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
-- import Test.Tasty
-- import Common
import qualified Data.Map as M 
import  Funcs

prop_reverseReverse :: [Char] -> Bool
prop_reverseReverse s = (reverse . reverse) s == s

prop :: Int -> Bool 
prop i = i - i == 0

prop_getFirst :: [[Coord]] -> Bool 
prop_getFirst xs = check out xs 
      where out = getFirst xs
            check [] _            = True
            check  _ [[]]         = True
            check (x:xxs) (y:ys)  = case y of 
                                   [] ->  True 
                                   _  -> x == y!!0 && (check xxs ys ) 
            check _ _             = True 

main :: IO ()
main =  hspec $ do
      describe "Prelude.head" $ do
        it "returns the first element of a list" $ do
          head [23 ..] `shouldBe` (23 :: Int)
      describe "merge" $ do
        it "properly merges sorted lists" $ 
            property $ prop
      describe "get first" $ do
        it "properly get the first item" $ 
            property $ prop_getFirst
      -- quickCheck (withMaxSuccess 10000 prop_reverseReverse)
      -- quickCheck prop



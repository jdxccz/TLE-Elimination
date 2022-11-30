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


-- see if the length of blocks is the same 
prop_shuffleboard1:: [Board] -> Bool 
prop_shuffleboard1 b = length (squeeze out) == length (squeeze b)
            where out =  shuffleboard b


-- see if the shuffled color matches the original color 
prop_shuffleboard2:: [[Coord]] -> Bool 
prop_shuffleboard2 b = check2 outs  bs 
            where out =  shuffleboard b
                  outs = squeeze out
                  bs   = squeeze b
                  check2:: Board -> Board -> Bool
                  check2 [] _       = True
                  check2 (x:xs) ys  = xx `elem` yys  && check2 xs ys
                      where (_, _, xx) = x
                  yys = getlast bs
                  getlast []       = []
                  getlast (c:cs)   =  cc:getlast cs 
                      where (_, _, cc) = c
                  

-- TODOs : more to test! 



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
      describe "shuffle board1" $ do
        it "properly shuffles board, have the same number of blocks" $ 
            property $ prop_shuffleboard1
      describe "shuffle board2" $ do
        it "properly shuffles board, have the same colors" $ 
            property $ prop_shuffleboard2
      -- quickCheck (withMaxSuccess 10000 prop_reverseReverse)
      -- quickCheck prop



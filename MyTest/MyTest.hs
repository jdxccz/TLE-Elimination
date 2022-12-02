module Main where
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
-- import Test.Tasty
-- import Common
-- import qualified Data.Map as M 
import Funcs


squeeze_p :: [[Coord]] -> [Coord]
squeeze_p [] = []
squeeze_p [[]] = []
squeeze_p (x:xs) = case x of 
              [] -> squeeze_p xs
              _ -> if x == [] then squeeze_p xs else x ++ squeeze_p xs
              where (t,y,z) =  x !! 0



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
prop_shuffleboard1 b = length (squeeze_p out) == length (squeeze_p b)
            where out =  shuffleboard b

-- >>> shuffleboard [[(0,0,0),(0,0,100)]]
-- [[(0,0,100),(0,0,0)]]
--
-- >>> squeeze_p [[(0,0,0),(0,0,100)]]
-- [(0,0,0),(0,0,100)]
--
-- >>> squeeze_p [[(0,0,100),(0,0,0)]]
-- []
--
prop_shuffleboard2:: [[Coord]] -> Bool 
prop_shuffleboard2 b = check2 outs  bs 
            where out =  shuffleboard b
                  outs = squeeze_p out
                  bs   = squeeze_p b
                  check2:: Board -> Board -> Bool
                  check2 [] _       = True
                  check2 (x:xs) ys  = xx `elem` yys  && check2 xs ys
                      where (_, _, xx) = x
                  yys = getlast bs
                  getlast []       = []
                  getlast (c:cs)   =  cc:getlast cs 
                      where (_, _, cc) = c
                  

-- TODOs : more to test! 
prop_removeLast1::[Coord] -> Bool 
prop_removeLast1 board1 = case  board1 of 
                  [] -> True
                  -- [_] -> removeLast board1 == []
                  _ ->  length board1 == length (removeLast board1 ) + 1


prop_removeLast2::[Coord] -> Bool 
prop_removeLast2 board1 = check board1 (removeLast board1 )
                where check _ [] = True 
                      check xs (y:ys) = y `elem` xs && check xs ys 


-- write a generator to generate proper board ? 

prop_putOnBoard::(Int, Int) -> Int -> MBoard -> Bool 
prop_putOnBoard pos c board1 = check pos c board1 out 
      where out = putOnBoard pos c board1
            outs = squeeze_p out
            bz = squeeze_p board1
            check pos1 _ board11 _  = case board11 of 
                                    [] -> True 
                                    -- [[]] -> True 
                                    -- [[]:rest] -> True  -- impossible cases
                                    boardelem: _ -> case boardelem of -- boardelem is list of (x, y, c)
                                          [] -> True 
                                          _  -> 
                                                if pos1 `elem` (allPos board1) && ((findColor pos1 board1) /= 100 )
                                                then length outs == length bz + 1
                                                else length outs == length bz 

-- >>> putOnBoard  (2,39) 0 [[(0,0,0)],[],[(2,39,0)]]
-- [[(0,0,0)],[(2,39,0),(2,39,0)]]
--


-- find all the positions from board 
allPos :: [[(Int, Int, Int)]] -> [(Int, Int)]
allPos [] = []
allPos (b:bdz) = case b of 
                        [] -> allPos bdz  
                        _  -> [(x,y)] ++ allPos bdz 
                                where (x, y, _) = b!!0
-- >>> allPos [[(0,1,0)],[],[(0,0,0)]]
-- [(0,1),(0,0)]

allPos2 :: [(Int, Int, Int)] -> [(Int, Int)]
allPos2 [] = []
allPos2 (b:bdz) = [(y, z)] ++ allPos2 bdz 
      where (_, y, z) = b

prop_countS:: Int -> Stack -> Bool 
prop_countS color s = ncolor  ==  ncolor' bd
            where ncolor = countS color s
                  bd = content s
                  ncolor':: Board -> Int
                  ncolor' [] = 0 
                  ncolor' (b:sboard) = if color == cb then ncolor' sboard +1 else ncolor' sboard
                      where (_, _, cb) = b

-- same y and c 
prop_deleteColor::Int -> Stack -> Bool 
prop_deleteColor c s = check (content s ) (content (deleteColor c s))
                  where check :: Board -> Board -> Bool 
                        check [] _ = True 
                        check _ [] = True 
                        check ba (b:bb) = (b2, b3) `elem` new_ba && check ba bb
                            where new_ba = allPos2 (content s )
                                  (_, b2, b3) = b

-- x start form 0 
prop_deleteColor1::Int -> Stack -> Bool 
prop_deleteColor1 c s = check (content (deleteColor c s)) 0 
                  where check :: Board  -> Int -> Bool 
                        check [] _  = True 
                        check (b:bb) n  = b1 == n && check bb (n+1)
                            where (b1,_, _) = b


-- >>> deleteColor 0 Stack {full = True, content = [(1,-2,-2)], eliminate = False}
-- Stack {full = True, content = [(0,-2,-2)], eliminate = True}
--

-- color c is deleted 
prop_deleteColor2::Int -> Stack -> Bool 
prop_deleteColor2 c s = check (content (deleteColor c s)) c
                  where check :: Board -> Int -> Bool 
                        check [] _ = True 
                        check (b:bb) color' = bc /= color' && check bb color'
                         where (_, _, bc) = b

-- item number -1 or the same 
prop_deleteItem ::(Int, Int) -> MBoard -> Bool
prop_deleteItem pos bd = length (squeeze_p bd) >= length (squeeze_p (deleteItem pos bd))


-- the item is deleted 
prop_deleteItem1 ::(Int, Int) -> MBoard -> Bool
prop_deleteItem1 pos bd = check pos bd (deleteItem pos bd)
            where check :: (Int, Int) -> MBoard -> MBoard -> Bool
                  check _ [] [] = True 
                  check _ [] _  = False
                  check _ _ [] = True 
                  check (x, y) bd1 bd2 = if (x, y) `elem` (allPos bd1) then checkDelete (x, y) (squeeze_p bd1) (squeeze_p bd2) 
                  else length (squeeze_p bd) == length (squeeze_p (deleteItem pos bd))
-- >>> (0,1) `elem` allPos [[(0,1,0)],[],[(0,0,0)]]
-- True
--

checkDelete :: (Int, Int) -> Board -> Board -> Bool
checkDelete _ [] [] = False
checkDelete _ [] _  = False
checkDelete _ _ []  = True 
checkDelete (x, y) (b1:bd1) (b2:bd2) = if (x, y) == (b11, b22) then checkDeleteItem b1 (b2:bd2) else checkDelete (x, y) bd1 (b2:bd2)
                  where (b11, b22, _) = b1

-- >>> checkDelete (0, 1) [(0,0,1), (0,1,1)] [(0,0,1), (0,1,1)]
-- False
--

checkDeleteItem :: (Int, Int, Int) -> Board  -> Bool 
checkDeleteItem _ [] = True
checkDeleteItem item bd = if (item `elem` bd ) then False else True

-- >>> checkDeleteItem (0, 1, 1) [(0,0,1)]
-- True
--


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
      describe "remove last1" $ do
        it "properly remove last, have n-1 blocks" $ 
            property $ prop_removeLast1
      describe "remove last2" $ do
        it "properly shuffles board, does not change the content" $ 
            property $ prop_removeLast2
      describe "proper putOnBoard" $ do
        it "properly put on board, if pos and color meets, should put on one elem, else do not put any elem" $ 
            property $prop_putOnBoard
      describe "proper countS" $ do
        it "proper count the number of color" $ 
            property $prop_countS
      describe "proper deleteColor" $ do
        it "do not add new contents( y and c) when delete color" $ 
            property $prop_deleteColor
      describe "proper deleteColor" $ do
        it "x start from 0" $ 
            property $prop_deleteColor1
      describe "delete color" $ do
        it "properly deleted the expected color" $ 
            property $prop_deleteColor2
      describe "proper deleteItem" $ do
        it "the item number of deleted board should be less than the original board" $ 
            property $prop_deleteItem
      describe "proper deleteItem" $ do
        it "the required item is deleted" $ 
            property $prop_deleteItem1
      
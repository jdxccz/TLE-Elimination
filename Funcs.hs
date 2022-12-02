module Funcs where
import Graphics.Gloss
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Set.Extra as Set
import Data.List as L
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import qualified System.Random as Random
import Graphics.Gloss.Juicy
import System.Random (Random(..), newStdGen)
import System.Random.Shuffle (shuffle')
import Control.Concurrent.ParallelIO.Global
import Test.QuickCheck

type Coord = (Int, Int, Int)


data Stack = Stack { full :: Bool
                   , content :: Board
                   , eliminate :: Bool
                   } deriving (Eq, Show)

-- should initialize the type for stack if we want to randomly generate test cases of stacks 
instance Arbitrary Stack where
   arbitrary = do
     full1 <- arbitrary
     content1 <- arbitrary
     eliminate1 <- arbitrary
     return $ Stack full1 content1 eliminate1

type Board = [Coord]
type MBoard = [[Coord]]

-- the next block to be move from board to stack 
type ToMove =  Coord

data World = World { -- nextMove :: ToMove
                     wMousePos:: Point
                   , mouseGridPos:: (Int, Int)
                   , stack :: Stack
                   , board :: MBoard
                   , score :: Int
                   , random :: Random.StdGen
                   , status :: Int
                   , showtext :: String
                   , mouseEnter ::Bool
                   , canUndoLast :: Bool
                   , pics :: [Picture]
                   } deriving (Show)


-- init_ran = Random.getStdGen 137
-- init_pics = []
-- instance Arbitrary World where
--    arbitrary = do
--      point <- arbitrary
--      mgrid <- arbitrary
--      initstack1 <- arbitrary
--      initboard1 <- arbitrary
--      initscore1 <- arbitrary
--      return $ (World point mgrid initstack1 initboard1 initscore1 init_ran  0 "" False False init_pics)

-- Number of blocks per row/column on the game board
blockNumber :: Int
blockNumber = 9

-- Number of picture layers on the game board
layers :: Int
layers = 3

-- Side length of the game board
boardSize :: Float 
boardSize = 500.0

-- Side length of the picture blocks
blockSize :: Float
blockSize = 40

-- Maximum capacity of the stack
-- stackLength:: Int
-- stackLength = 7

getFirst :: [[Coord]] -> [Coord]
getFirst [] = []
getFirst (x:xs) = x!!0:getFirst xs


shuffleboard :: [Board] -> [Board]
shuffleboard g = newBoard
  --g {board = newBoard, canUndoLast = False}
  where newBoard = replace (shufflev (squeeze g)) g

squeeze :: [[Coord]] -> [Coord]
squeeze [] = []
squeeze [[]] = []
squeeze (x:xs) = case x of 
              [] -> squeeze xs
              _ -> if z == 100 || x == [] then squeeze xs else x ++ squeeze xs
              where (t,y,z) =  case x  of 
                                  -- [] -> (-1, -1, -1)
                                  _  -> x !! 0


-- >>> squeeze [[(0,1,0)],[],[(0,0,0)]]
-- [(0,1,0),(0,0,0)]


shufflev :: Board -> Board
shufflev b = shuffle' b (length b) pureGen
  where pureGen = mkStdGen 137

replace :: Board -> MBoard -> MBoard
replace _ [] = []
replace bs (mb:mbs) = if z == 100 then mb: (replace bs mbs) else mb2 : (replace (drop (length mb) bs ) mbs)
  where (x,y,z) =  case mb  of 
                      [] -> (-1, -1, -1)
                      _  -> mb !! 0
        mb2 = replacev bs mb

replacev :: Board -> Board -> Board
replacev _ [] = []
replacev ((x,y,z):bs) ((a,b,c):ts) = (a,b,z):(replacev bs ts)

cleanStack :: World -> World
cleanStack g = g {stack = newStack, score = newScore, status = newStatus, canUndoLast = False}
  where
    newStack = Stack False [(0,0,0)] False
    newStatus = if newScore == (blockNumber * blockNumber * layers) then 2 else status g
    newScore = score g + (length (content (stack g))) - 1

undoLastMove :: World -> World
undoLastMove g = case canUndoLast g of
  False -> g
  True -> g {board = newBoard, stack = newStack, canUndoLast = False}
  where
    newStack = Stack (full (stack g)) (removeLast (content (stack g))) (eliminate (stack g))
    newBoard = putOnBoard (mouseGridPos g) (lastColor (content (stack g))) (board g)

lastColor :: Board -> Int
lastColor [] = 100
lastColor [(_, _, c)] = c
lastColor (x:xs) = lastColor xs

removeLast :: Board -> Board
removeLast [] = []
removeLast [_] = []
removeLast (x:xs) = x : removeLast xs

putOnBoard :: (Int, Int) -> Int -> MBoard -> MBoard
putOnBoard _ 100 b = b
putOnBoard _ _ [] = []
putOnBoard pos c (boardelement:theboard) = case boardelement of 
                                    [] -> putOnBoard pos c theboard
                                    _  -> if pos /= (x,y) 
                                            then 
                                              boardelement:(putOnBoard pos c theboard)  -- put to the next position 
                                            else if (z == 100) 
                                              then 
                                                [(x,y,c)]:theboard 
                                              else 
                                                ((x,y,c):boardelement):theboard
                                            where (x,y,z) = boardelement !! 0 
                                            -- TODO what if board == [[]]
                                            -- What is the structure of MBoard

-- >>> putOnBoard (0,0) 0 [[(0,1,0)],[],[(0,0,0)]]
-- [[(0,1,0)],[(0,0,0),(0,0,0)]]


-- no need to check
countS :: Int -> Stack -> Int
countS it (Stack _ [] _) = 0
countS it (Stack first ((x, y, c):xs) third) = if it == c then (countS it (Stack first xs third)) + 1 else countS it (Stack first xs third)


deleteColor :: Int -> Stack -> Stack
deleteColor nowColor (Stack full content eliminate) = Stack full (updateContent (deleteContent nowColor content) 0) True

deleteContent :: Int -> Board -> Board
deleteContent nowColor [] = []
deleteContent nowColor ((x, y, c):xs) = if c == nowColor then deleteContent nowColor xs else (x, y, c) : (deleteContent nowColor xs)

updateContent :: Board -> Int -> Board
updateContent [] state = []
updateContent ((x, y, c):xs) state = (state, y, c):(updateContent xs (state + 1))

deleteItem :: (Int, Int) -> MBoard -> MBoard
deleteItem _ [] = []
deleteItem pos (boardelement: theboard) = case boardelement of 
    [] -> deleteItem pos theboard
    _  -> if pos /= (x, y) then boardelement : deleteItem pos theboard
                                            else if length(boardelement) == 1 then ([(x, y, 100)]: theboard)
                                            -- MBoard at last will have one [(x, y, 100)] for each position, so it will not be empty 
                                            else (drop 1 boardelement) : theboard
    where
      (x,y,z) = boardelement !! 0


addItem :: Int -> Board -> Board
addItem 0 xs = xs
addItem c xs = xs ++ [((length xs), 0, c)]


findColor :: (Int, Int) -> MBoard -> Int
findColor _ [] = 0
findColor pos (boardelement: theboard) = case boardelement of 
  [] ->  0
  _ -> if pos == (x, y) then z else findColor pos theboard
    where
      (x,y,z) = boardelement !! 0


gridFromScreen :: Point -> (Int, Int)
gridFromScreen (x, y) = (i, j)
    where
    xScaled = (x + boardSize /2) / (blockSize)
    yScaled = (y+ boardSize /2)/ (blockSize)  -- ç/ (blockSize) 
    i = fromIntegral (floor (xScaled ))
    j = fromIntegral (floor (yScaled ))


validPlacementCoord :: (Int, Int) -> Board ->  Bool
validPlacementCoord _ [] = False
validPlacementCoord pos ((x, y, z):board) = if pos == (x, y) then True else validPlacementCoord pos board

formboard :: [Coord] -> [[Coord]]
formboard (x:y:z:xs) = [x,y,z]:formboard xs
formboard [] = []

tricopy :: [(Int, Int)] -> [(Int, Int)]
tricopy [] = []
tricopy (x:xs) = [x,x,x] ++ tricopy xs

putToghther :: [(Int, Int)] -> [Int] -> [Coord]
putToghther [] _ = []
putToghther _ [] = []
putToghther ((x,y):boards) (c:colors)  = (x, y, c) : (putToghther boards colors)

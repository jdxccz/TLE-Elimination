-- module Main where

-- import Graphics.Gloss

-- main:: IO ()
-- main = putStr "Welcome to TLE World"


-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Main where
import Graphics.Gloss
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Set.Extra as Set
import Data.List as L
import Graphics.Gloss.Interface.Pure.Game
-- import Data.Monoid
import System.Random
import qualified System.Random as Random
import System.Random (Random(..), newStdGen)
import System.Random.Shuffle (shuffle')

-- main:: IO ()
-- main = putStr "Welcome to TLE World"

-- window :: Display
-- window = InWindow "Nice Window" (900, 900) (10, 10)

-- background :: Color
-- background = black

-- drawing :: Picture
-- drawing = pictures
--   [ color ballColor $ circleSolid 30
--   , color paddleColor $ rectangleSolid 10 50
--   ]
--   where
--     ballColor = dark red 
--     paddleColor = light (light blue) 


type Coord = (Int, Int, Int)
data Stack = Stack { full :: Bool
                   , content :: Board
                   , eliminate :: Bool
                   } deriving (Eq, Show)

-- type Grid = [(Int, Int)]

type Board = [Coord]
-- type Colors  = [Int]
type MBoard = [[Coord]]

-- the next block to be move from board to stack 
type ToMove =  Coord

data World = World { nextMove :: ToMove
                   , wMousePos:: Point
                   , mouseGridPos:: (Int, Int)
                   , stack :: Stack
                   , board :: MBoard
                   , score :: Int
                   , random :: Random.StdGen
                   , status :: Int
                   , showtext :: String
                   , mouseEnter ::Bool
                   , canUndoLast :: Bool
                   } deriving (Show)

boardNumber :: Int
boardNumber = 9

boardHeight :: Int
boardHeight = 3

size :: Float
size = 400.0

boardSize :: Float
boardSize = 40

stackLength:: Int
stackLength = 7

-- 画边框
drawGrid :: Int -> [Picture] -> [Picture]
drawGrid squares grid = -- grid = [] initially 
  if l < (fromIntegral squares) + 1 then
    drawGrid squares (grid ++
      [ Color red $ Line [(boardSize*l, 0), (boardSize*l, size)] -- line? Picture type? 
      , Color red $ Line [(0, boardSize*l), (size, boardSize*l)]
      ]
    )
  else grid
  where l = (fromIntegral $ length grid)/2


-- drawStackBlock:: Coord -> Picture
-- drawStackBlock (index, givencolor) =
--   Color c $ mconcat $ [polygon [(0, y), (0 + s, y), (0 + s, y + s), (0, y + s)] ]
--   where
--     s = boardSize
--     y = s * (fromIntegral index)
--     c 
--       | givencolor == 0 = white 
--       | givencolor == 1 = yellow
--       | givencolor == 2 = blue
--       | givencolor == 3 = green 
--       | givencolor == 4 = red
--       | otherwise = violet

drawBlock :: Coord -> Picture
drawBlock (x, y, givencolor) =
   Color c $ mconcat [ polygon [(x', y'), (x' + s, y'), (x' + s, y' + s), (x', y' + s)] ]
  where
    s = boardSize
    x' = s * (fromIntegral x)
    y' = s * (fromIntegral y)
    c
      | givencolor == 0 = white
      | givencolor == 1 = yellow
      | givencolor == 2 = blue
      | givencolor == 3 = green
      | givencolor == 4 = red
      | givencolor == 100 = white
      | otherwise = violet

getFirst :: [[Coord]] -> [Coord]
getFirst [] = []
getFirst (x:xs) = x!!0:getFirst xs

drawBoard :: [[Coord]] -> [Picture]
drawBoard coords = map drawBlock (getFirst coords)

drawStack :: [Coord] -> [Picture]
drawStack colors = map drawBlock (colors)

-- getRandom :: Random.RandomGen g => g -> Int -> (Int, g)
-- getRandom gen len = Random.randomR (0, len) gen 


drawWorld :: Int -> World -> Picture
drawWorld squares world =
  case (status world) of
    2 -> Translate (-size/2) (-size/2)
      $ Color black
      $ Scale 0.2 0.2
      $ Text ("You WIN!! Score: " ++ (show (score world)))
    1 -> Translate (-size/2) (-size/2) -- 游戏结束
      $ Color black
      $ Scale 0.2 0.2
      $ Text ("You LOSE!! Score: " ++ (show (score world)))
      --  Text ("mostpos: " ++ (show (mouseGridPos world)))
    _ ->
      Translate (-size/2) (-size/2)
      $ pictures
      $ -- (drawGrid squares []) ++
        drawBoard (board world)
        ++ drawStack (content (stack world)) -- picture可以叠加




handleEvent :: Event -> World -> World
-- handleEvent (EventKey (SpecialKey KeyDown) Down _ _) world =
--   world { stack = Stack False [(0,0,0)], board}
-- handleEvent (EventKey (SpecialKey KeyUp) Down _ _) world =
--   world { stack = Stack False [(0,0,0)]}
-- handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) world =
--   world { stack = Stack False [(0,0,0)]}
-- handleEvent (EventKey (SpecialKey KeyRight) Down _ _) world =
--   world {stack = Stack False [(0,0,0)] }
-- handleEvent _ world = world
handleEvent e g = case e of
    -- (EventKey (Char 'n') Down _ pos) -> (newGame (gmRandomSource g)) { gmMousePos = pos }
    (EventKey (MouseButton RightButton) Down _ pos) -> onMouseDown (onMouseMove pos g)
    -- (EventMotion pos) -> onMouseMove pos g
    (EventKey (SpecialKey KeySpace) Down _ _) -> cleanStack g
    (EventKey (SpecialKey KeyLeft) Down _ _) -> undoLastMove g
    (EventKey (SpecialKey KeyRight) Down _ _) -> shuffleboard g
    _ -> g

shuffleboard :: World -> World
shuffleboard g = g {board = newBoard, canUndoLast = False}
  where newBoard = replace (shufflev (squeeze (board g))) (board g)

squeeze :: MBoard -> Board
squeeze [] = []
squeeze (x:xs) = if z == 100 then squeeze xs else x ++ squeeze xs
  where (t,y,z) =  x !! 0

shufflev :: Board -> Board
shufflev b = shuffle' b (length b) pureGen
  where pureGen = mkStdGen 137

replace :: Board -> MBoard -> MBoard
replace _ [] = []
replace bs (mb:mbs) = if z == 100 then mb: (replace bs mbs) else mb2 : (replace (drop (length mb) bs ) mbs)
  where (x,y,z) =  mb !! 0
        mb2 = replacev bs mb

replacev :: Board -> Board -> Board
replacev _ [] = []
replacev ((x,y,z):bs) ((a,b,c):ts) = (a,b,z):(replacev bs ts)

cleanStack :: World -> World
cleanStack g = g {stack = newStack, score = newScore, status = newStatus, canUndoLast = False}
  where
    newStack = Stack False [(0,0,0)] False
    newStatus = if newScore == (boardNumber * boardNumber * boardHeight) then 2 else status g
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
putOnBoard pos c (boardelement:theboard) = if pos /= (x,y) 
  then 
    boardelement:(putOnBoard pos c theboard) 
  else if (z == 100) 
    then 
      [(x,y,c)]:theboard 
    else 
      ((x,y,c):boardelement):theboard
  where (x,y,z) = boardelement !! 0

onMouseMove :: Point -> World -> World -- point = (float, float)
onMouseMove p g = g { wMousePos = p }

onMouseDown :: World -> World
onMouseDown g = case validPlacementCoord mouseGridPos (getFirst (board g)) && (findColor mouseGridPos (board g) /= 100) of
    -- False -> g {mouseEnter = True}
    -- True -> g {mouseEnter = True}
    False -> g {mouseEnter = False}-- , status = 1}--  {showtext = "sth wrong"} --, status = 1
    True -> g { mouseEnter = True, mouseGridPos = mouseGridPos, stack = newStack, canUndoLast = newCanUndo }

    where
    mouseScreenPos = wMousePos g
    mouseGridPos = gridFromScreen mouseScreenPos  --  (Int, Int)
    -- deletePos = (3, 3) -- mouseGridPos
    -- foundColor = findColor deletePos  (board g)
    nowColor = findColor mouseGridPos (board g)
    newStack = if countS nowColor (stack g) == 2 then deleteColor nowColor (stack g) else (stack g)
    newCanUndo = not (countS nowColor (stack g) == 2)

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
deleteItem pos (boardelement: theboard) = if pos /= (x, y) then boardelement : deleteItem pos theboard
                                          else if length(boardelement) == 1 then ([(x, y, 100)]: theboard)
                                          else (drop 1 boardelement) : theboard
  where
    (x,y,z) = boardelement !! 0


addItem :: Int -> Board -> Board
addItem 0 xs = xs
addItem c xs = xs ++ [((length xs), 0, c)]


findColor :: (Int, Int) -> MBoard -> Int
findColor _ [] = 0
findColor pos (boardelement: theboard) = if pos == (x, y) then z else findColor pos theboard
  where
    (x,y,z) = boardelement !! 0



gridFromScreen :: Point -> (Int, Int)
gridFromScreen (x, y) = (i, j)
    where
    xScaled = (x + size /2) / (boardSize)
    yScaled = (y+ size /2)/ (boardSize)  -- ç/ (boardSize)
    -- xScaled = if (x-10) / (boardSize*10)  < 9 && (x-10) / boardSize > 2 then (x-10) / boardSize else 3
    -- yScaled = if (y+890) / (boardSize*10) < 9 && (y+890) / boardSize > 2 then (y+890) / boardSize else 4 
    i = fromIntegral (floor (xScaled ))
    j = fromIntegral (floor (yScaled ))



validPlacementCoord :: (Int, Int) -> Board ->  Bool
validPlacementCoord pos [] = False
validPlacementCoord pos ((x, y, z):board) = if pos == (x, y) then True else validPlacementCoord pos board

main :: IO ()
main = do
  ran <- Random.getStdGen
  play (InWindow "TLE" (round size + 20, round size + 20) (10, 10)) 
    white
    9
    (World (0,0,0) (0,0) (0,0) initstack initboard 0 ran 0 "" False False)
    (drawWorld squares)
    handleEvent  -- update world 
    (updateWorld squares)
  where
    initstack = Stack False [(0,0,0)] False
    squares = round (size/boardSize)
    pureGen = mkStdGen 137
    initboard = initBoard pureGen boardNumber



initBoard :: Random.StdGen -> Int -> MBoard
initBoard ran boardNum = formboard coloredboard
      where initboard0 = [(i,j) | i <- [2.. boardNum], j <- [2.. boardNum]]
            pureGen = mkStdGen 137
            rolls n =  take n . unfoldr (Just . uniformR (1, 5))
            initcolors0 = rolls (boardNum * boardNum) pureGen
            initcolors1 = initcolors0 ++ initcolors0 ++ initcolors0
            initcolors = shuffle' initcolors1 (boardNum * boardNum * boardHeight) pureGen
            initboard = tricopy initboard0
            -- addRandNum (x, y) = (x, y, head (rolls 1 pureGen ))
            -- coloredboard = map addRandNum initboard -- [(i,j,c) | (i,j) <- initboard, c <- initcolors]  -- [(x, y, color)]
            coloredboard = putToghther initboard initcolors
            -- convertListToTuple [x, y, z] = (x, y, z)
            -- convertListToTuple _ = (0, 0, 0)
            -- reformedboard = map convertListToTuple coloredboard

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

updateWorld :: Int -> Float -> World -> World
updateWorld n _ world
     | mouseEnter world == True  =  world { score = newScore, board = newBoard, stack = newStack, status = newStatus, mouseEnter = False, mouseGridPos = newmouseGridPos}
     | otherwise = world
       where
          mouseScreenPos = wMousePos world
          newmouseGridPos = gridFromScreen mouseScreenPos  --  (Int, Int)
          deletePos = newmouseGridPos  -- mouseGridPos
          foundColor = findColor deletePos  (board world )
          newStack = if length (content (stack world )) == 7 then Stack True [] False
                     else if (eliminate (stack world)) || (foundColor == 100) then Stack False (content (stack world )) False
                     else Stack False (addItem foundColor (content (stack world ))) False
          newStatus = if score world == (boardNumber * boardNumber * boardHeight - 3) && (eliminate (stack world)) then 2 else if full newStack || status world == 1  then  1 else 0
          newBoard = deleteItem deletePos (board world )
          newScore = if (eliminate (stack world)) then (score world) + 3 else (score world)
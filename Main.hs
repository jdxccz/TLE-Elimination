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
import System.Random
import qualified System.Random as Random
import Graphics.Gloss.Juicy
import System.Random (Random(..), newStdGen)
import System.Random.Shuffle (shuffle')
import Control.Concurrent.ParallelIO.Global


type Coord = (Int, Int, Int)
data Stack = Stack { full :: Bool
                   , content :: Board
                   , eliminate :: Bool
                   } deriving (Eq, Show)

type Board = [Coord]
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
                   , pics :: [Picture]
                   } deriving (Show)

-- Number of blocks per row/column on the game board
blockNumber :: Int
blockNumber = 9

-- Number of picture layers on the game board
layers :: Int
layers = 3

-- Side length of the game board
boardSize :: Float 
boardSize = 400.0

-- Side length of the picture blocks
blockSize :: Float
blockSize = 40

-- Maximum capacity of the stack
stackLength:: Int
stackLength = 7


backgroundColor :: Color
backgroundColor = makeColorI 154 232 125 255

boardFrameColor :: Color 
boardFrameColor =  makeColorI 207 160 91 255

boardBackColor :: Color
boardBackColor =  makeColorI 163 104 15 255

stackColor :: Color
stackColor = makeColorI 237 200 145 255

bgFrameSize :: Float
bgFrameSize = 300.0

bgBackSize :: Float
bgBackSize = 250.0

drawBackGround :: [Picture]
drawBackGround = map (Scale 0.8 0.8) [manual, title, boardBG]
  where title = (Translate (-50) 650.0) $ (Scale 0.8 0.8) $ Color boardBackColor $ Text "TLE Elimination"
        boardBG = (Translate 275.0 275.0) $ Pictures $ [ Color boardFrameColor $ polygon [(-bgFrameSize, -bgFrameSize), (-bgFrameSize, bgFrameSize), (bgFrameSize, bgFrameSize), (bgFrameSize, -bgFrameSize)]]
                  ++ [Color boardBackColor $ polygon [(-bgBackSize, -bgBackSize), (-bgBackSize, bgBackSize), (bgBackSize, bgBackSize), (bgBackSize, -bgBackSize)]] 
        stackBG = Color stackColor $ Pictures $ map drawSubCircle [1..5]
        
drawSubCircle :: Int -> Picture
drawSubCircle a = Translate ((fromIntegral(a))*200.0) (100.0) $ ThickCircle 10.0 255.0


drawBlock :: Coord -> Picture
drawBlock (x, y, givencolor) =
   Color c $ polygon [(x', y'), (x' + s, y'), (x' + s, y' + s), (x', y' + s)]
  where
    s = blockSize
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

drawBlock2 :: [Picture] -> Coord  -> Picture
drawBlock2 pics (x, y, 100)= Color boardBackColor $ polygon [(x', y'), (x' + s, y'), (x' + s, y' + s), (x', y' + s)]
 where 
    s = blockSize
    x' = s * (fromIntegral x)
    y' = s * (fromIntegral y)
 
drawBlock2 pics (x, y, picsIndex)=  pic
  where 
    blockframe = Color black $ polygon [(x', y'), (x' + s, y'), (x' + s, y' + s), (x', y' + s)] 
    pic = Translate (x'+s/2) (y'+s/2) $ Scale 0.14 0.14 $ pics!!picsIndex
    s = blockSize
    x' = s * (fromIntegral x)
    y' = s * (fromIntegral y)

manualblock :: Picture
manualblock = Color white $ Polygon [(x-100, y), (x-100, y+750),(x+2300, y+750), (x+2300, y+0)]
  where x = -400
        y = 450

manual :: Picture
manual = Translate (-430) (0) $ Scale 0.15 0.15 $ Pictures [manualblock, t, t1, t2, t3]
  where 
    t = Translate x (600+y) $ Text "Props:"
    t1 = Translate x (450+y) $ Text "Clear the stack: space"
    t2 = Translate x (300+y) $ Text "Shuffle the board: right arrow" 
    t3 = Translate x (150+y) $ Text "Revoke the last move: left arrow"  
    x = -400 
    y = 400

getFirst :: [[Coord]] -> [Coord]
getFirst [] = []
getFirst (x:xs) = x!!0:getFirst xs

drawBoard :: [[Coord]] -> [Picture] -> [Picture]
drawBoard coords pics = map (drawBlock2 pics) (getFirst coords)

drawStack :: [Coord]  -> [Picture] -> [Picture]
drawStack colors pics  = [Translate (40) (-20) $ mconcat (map (drawBlock2 pics) (colors))] ++ [stacktail]
  -- where stackBG = Color black $ Pictures $ map drawSubCircle [1..5]
  where stacktail = Translate (40) (-20) $ drawBlock2 pics (8, 0,13)



drawWorld :: Int -> World -> Picture
drawWorld _ world =
  case (status world) of
    2 -> Translate (-boardSize/2) (-boardSize/2)
      $ Color black
      $ Scale 0.2 0.2
      $ Text ("You WIN!! Score: " ++ (show (score world)))
    1 -> Translate (-boardSize/2) (-boardSize/2) -- 游戏结束
      $ Color black
      $ Scale 0.2 0.2
      $ Text ("You LOSE!! Score: " ++ (show (score world)))
      --  Text ("mostpos: " ++ (show (mouseGridPos world)))
    _ ->
      Translate (-boardSize/2) (-boardSize/2) 
      $ pictures
      $ drawBackGround
       ++ drawBoard (board world) (pics world)
       ++ drawStack (content (stack world)) (pics world) -- picture可以叠加


handleEvent :: Event -> World -> World
handleEvent e g = case e of
    -- (EventKey (Char 'n') Down _ pos) -> (newGame (gmRandomSource g)) { gmMousePos = pos }
    (EventKey (MouseButton LeftButton) Down _ pos) -> onMouseDown (onMouseMove pos g)
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
    xScaled = (x + boardSize /2) / (blockSize)
    yScaled = (y+ boardSize /2)/ (blockSize)  -- ç/ (blockSize)
    -- xScaled = if (x-10) / (blockSize*10)  < 9 && (x-10) / blockSize > 2 then (x-10) / blockSize else 3
    -- yScaled = if (y+890) / (blockSize*10) < 9 && (y+890) / blockSize > 2 then (y+890) / blockSize else 4 
    i = fromIntegral (floor (xScaled ))
    j = fromIntegral (floor (yScaled ))



validPlacementCoord :: (Int, Int) -> Board ->  Bool
validPlacementCoord pos [] = False
validPlacementCoord pos ((x, y, z):board) = if pos == (x, y) then True else validPlacementCoord pos board

initBoard :: Random.StdGen -> Int -> MBoard
initBoard pureGen blockNum = formboard coloredboard
      where initboard0 = [(i,j) | i <- [1.. blockNum], j <- [1.. blockNum]]
            rolls n =  take n . unfoldr (Just . uniformR (1, 12))
            initcolors0 = rolls (blockNum * blockNum) pureGen
            initcolors1 = initcolors0 ++ initcolors0 ++ initcolors0
            initcolors = shuffle' initcolors1 (blockNum * blockNum * layers) pureGen
            initboard = tricopy initboard0
            -- addRandNum (x, y) = (x, y, head (rolls 1 pureGen ))
            -- coloredboard = map addRandNum initboard -- [(i,j,c) | (i,j) <- initboard, c <- initcolors]  -- [(x, y, color)]
            coloredboard = putToghther initboard initcolors


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
updateWorld _ _ world
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
          newStatus = if score world == (blockNumber * blockNumber * layers - 3) && (eliminate (stack world)) then 2 else if full newStack || status world == 1  then  1 else 0
          newBoard = deleteItem deletePos (board world )
          newScore = if (eliminate (stack world)) then (score world) + 3 else (score world)

filepaths :: [String]
filepaths = map (\a -> "pics/" ++ (show a) ++ ".PNG") [1..14] -- todo


picIO :: String -> IO Picture
picIO filepath =  do 
          p <- loadJuicyPNG filepath
          case p of
            Just p -> return p

loadpictures :: IO [Picture]
loadpictures = parallel $ map picIO filepaths

main :: IO ()
main = do
  ran <- Random.getStdGen
  pics <- loadpictures
  play --FullScreen
   (InWindow "TLE" (800, 800) (10, 10)) 
    backgroundColor
    9
    (World (0,0,0) (0,0) (0,0) initstack initboard 0 ran 0 "" False False pics)
    (drawWorld 0)
    handleEvent  -- update world 
    (updateWorld 0)
  where
    initstack = Stack False [(0,0,0)] False
    pureGen = mkStdGen 137
    initboard = initBoard pureGen blockNumber


window :: Display
window = InWindow "try" (500, 500) (10, 10)



-- main :: IO ()
-- main = do 
--     pics  <- loadpictures -- pics should be a list of picture here
--     display window white (Scale 0.1 0.1 $ Translate 100 100 $ Pictures [pics!!0, Scale 0.1 0.1 $ Translate 100 100 $pics!!1] )

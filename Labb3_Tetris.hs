{- |
Module      : Tetris
Description : The Tetris game (main module)
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : <Allan Khaledi, Ali Berat Can>
Lab group   : <58>
-}

module Main where

import ConsoleGUI
-- import ThreepennyGUI  -- either use ConsoleGUI or ThreepennyGUI

import Shapes
import Shapes (prop_Shape)
import Data.Maybe

--------------------------------------------------------------------------------
-- * The code that puts all the piece together
main :: IO ()
main = runGame tetrisGame

tetrisGame :: Game Tetris
tetrisGame = Game 
  { startGame     = startTetris
  , stepGame      = stepTetris
  , drawGame      = drawTetris
  , gameInfo      = defaultGameInfo prop_Tetris
  , tickDelay     = defaultDelay
  , gameInvariant = prop_Tetris 
  }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation

type Piece = (Pos, Shape)
type Pos   = (Int, Int)

-- | The state of the game consists of three parts:
data Tetris = Tetris 
  { piece  :: Piece    -- ^ The position and shape of the falling piece
  , well   :: Shape    -- ^ The well (the playing field), where the falling pieces pile up
  , shapes :: [Shape]  -- ^ An infinite supply of random shapes
  }

-- | The size of the well
wellWidth, wellHeight :: Int
wellWidth  = 10
wellHeight = 20

wellSize :: (Int, Int)
wellSize   = (wellHeight, wellWidth)

-- | Starting position for falling pieces
startPosition :: Pos
startPosition = (0, wellWidth `div` 2 - 1)

-- | Pos addition
add :: Pos -> Pos -> Pos
(h1, w1) `add` (h2, w2) = (h1 + h2, w1 + w2)

-- | Move the falling piece into position
place :: (Pos, Shape) -> Shape
place (v, s) = shiftShape v s

-- B4 | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (_, piece) well (nextPiece:supply)) = 
  prop_Shape piece && shapeSize well == wellSize

-- B5 | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls shape = Shape (topWall : sideWalls ++ [bottomWall])
  where
    numRows = length (rows shape)
    numCols = length (head (rows shape))
    
    blackRow = replicate (numCols + 2) (Just Black)
    topWall = blackRow
    bottomWall = blackRow
    
    sideWalls = [Just Black : row ++ [Just Black] | row <- rows shape]

-- B6 | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v, p) w _) = addWalls (combine (shiftShape v p) w)

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, piece) well supply
 where
  well         = emptyShape wellSize
  (piece:supply) = map selectShape rs 
  selectShape r = allShapes !! (floor (r * fromIntegral (length allShapes)))

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.

-- B7
move :: (Int, Int) -> Tetris -> Tetris
move (dh, dw) (Tetris ((h, w), shape) well shapes) =
  Tetris ((h + dh, w + dw), shape) well shapes

-- B8
-- tick :: Tetris -> Maybe (Int, Tetris)
-- tick t = Just (0, move (1, 0) t)

-- stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
-- stepTetris Tick t = tick t
-- stepTetris action t = Just (0, t)

-- C1
collision :: Tetris -> Bool
collision (Tetris ((h, w), shape) well _ ) =
  h < 0 || h + numRows > wellHeight || w < 0 || w + numCols > wellWidth || 
  placedShape `overlaps` shape  || placedShape `overlaps` well  || 
  shape `overlaps` well 
  where
    placedShape = place ((h, w), shape)
    (numRows, numCols) = shapeSize shape

{- tick :: Tetris -> Maybe (Int, Tetris)
tick t 
  | collision newTetris = Just (0, t)
  | otherwise = Just (0, newTetris)
  where
    newTetris = move (1,0) t -}

-- C2
-- stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
-- stepTetris Tick t = tick t
-- stepTetris MoveDown t = tick t
-- stepTetris action t = Just (0, t)

-- C3
-- stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
-- stepTetris Tick t        = tick t
-- stepTetris MoveDown t    = tick t
-- stepTetris (MoveLeft) t  = Just (0, movePiece (-1) t)
-- stepTetris (MoveRight) t = Just (0, movePiece 1 t)
-- stepTetris action t      = Just (0, t)

movePiece :: Int -> Tetris -> Tetris
movePiece dw t 
  | collision newTetris = t 
  | otherwise = newTetris
  where
    newTetris = move (0, dw) t

-- C4
rotate :: Tetris -> Tetris
rotate t 
  | collision newTetris = t 
  | otherwise = newTetris 
  where
    newTetris = Tetris (pos, rotateShape (snd (piece t))) (well t) (shapes t)
    pos = fst (piece t)

-- C6
rotatePiece :: Tetris -> Tetris
rotatePiece t 
  | collision newTetris = t 
  | otherwise = newTetris 
  where
    newTetris = Tetris (pos, rotateShape (snd (piece t))) (well t) (shapes t)
    pos = fst (piece t)

stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris Tick t        = tick t
stepTetris MoveDown t    = tick t
stepTetris (MoveLeft) t  = Just (0, movePiece (-1) t)
stepTetris (MoveRight) t = Just (0, movePiece 1 t)
stepTetris (Rotate) t    = Just (0, rotatePiece t)

-- C7
tick :: Tetris -> Maybe (Int, Tetris)
tick t 
  | collision newTetris = dropNewPiece t
  | otherwise = Just (0, newTetris)
  where
    newTetris = move (1,0) t


-- C9
isComplete :: Row -> Bool
isComplete = all isJust

clearLines :: Shape -> (Int, Shape)
clearLines (Shape rows) =
  let completedRows = filter isComplete rows
      numCompletedRows = length completedRows
      remainingRows = replicate (length completedRows) (emptyRow (length (head rows)))
      newRows = remainingRows ++ filter (not . isComplete) rows
  in (numCompletedRows, Shape newRows)

-- C10
{- dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece t@(Tetris (pos, piece) well (nextPiece:shapes))
  | collision newTetris = Just (0, t) 
  | otherwise = Just (numClearedRows, newTetris)
  where
    newWell = combine (place (pos, piece)) well
    (numClearedRows, clearedWell) = clearLines newWell
    newTetris = Tetris (startPosition, nextPiece) clearedWell shapes -}
dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece t@(Tetris (pos, piece) well (nextPiece:supply))
  | collision newTetris || pieceAboveTop newTetris = Nothing  -- End the game if there's a collision or the piece is above the top.
  | otherwise = Just (clearedRows, newTetris)
  where
    newWell = combine (place (pos, piece)) well
    (clearedRows, newWellAfterClear) = clearLines newWell
    newTetris = Tetris (startPosition, nextPiece) newWellAfterClear supply

-- Define a helper function to check if the piece is above the top of the well.
pieceAboveTop :: Tetris -> Bool
pieceAboveTop (Tetris ((_, y), _) _ _) = y < 0

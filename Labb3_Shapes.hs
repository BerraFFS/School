{- |
Module      : Shapes
Description : Types and functions for shapes. The list of all tetris pieces.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : Allan Khaledi, Ali Berat Can
Lab group   : 58
-}

module Shapes where

import Data.List (transpose)
import Data.Maybe (isNothing, isJust)
import Test.QuickCheck

-- * Shapes

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

type Square = Maybe Colour

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.
type Row   = [Square]
data Shape = Shape { rows :: [Row] } deriving Eq

-- * Showing shapes
showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
 where
  showRow r = [showSquare s | s <- r]
    
  showSquare Nothing      = '.'
  showSquare (Just Black) = '#' -- can change to '█' on linux/mac
  showSquare (Just Grey)  = 'g' -- can change to '▓'
  showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [Shape (makeSquares s) | s <- shapes] 
 where
   makeSquares = map (map colour)
   colour c    = lookup c [ ('I', Red),  ('J', Grey),  ('T', Blue), ('O', Yellow)
                          , ('Z', Cyan), ('L', Green), ('S', Purple) ]
   shapes = [["I",
              "I",
              "I",
              "I"],
             [" J",
              " J",
              "JJ"],
             [" T",
              "TT",
              " T"],
             ["OO",
              "OO"],
             [" Z",
              "ZZ",
              "Z "],
             ["LL",
              " L",
              " L"],
             ["S ",
              "SS",
              " S"]]

-- * Some simple functions

-- ** A1
emptyShape :: (Int, Int) -> Shape
emptyShape (rows, cols) = Shape (replicate rows (emptyRow cols)) 

emptyRow :: Int -> Row
emptyRow cols = replicate cols Nothing

-- ** A2

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int, Int)
shapeSize (Shape rows) = (numRow, numCol)
  where
    numRow = length rows
    numCol = length (head rows)

-- ** A3

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (Shape rows) = length [x | x <- concat rows, x /= Nothing]

-- * The Shape invariant

-- ** A4
-- | Shape invariant (shapes have at least one rows, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape (Shape rows) = length rows > 0 && sameLengthRows rows
  where
    sameLengthRows [] = True
    sameLengthRows (r:rs) = all (== length r) (map length rs)

-- * Test data generators

-- ** A5
-- | A random generator for colours
genColour :: Gen Colour
genColour = elements [Black, Red, Grey, Blue, Yellow, Cyan, Green, Purple]

instance Arbitrary Colour where
  arbitrary = genColour

-- ** A6
-- | A random generator for shapes
genShape :: Gen Shape
genShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = genShape

-- * Transforming shapes

-- ** A7
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (Shape []) = Shape []
rotateShape (Shape rows) = let rotateRows = transpose rows in Shape (map reverse rotateRows)

-- ** A8
-- | shiftShape adds empty squares above and to the left of the shape

shiftRowRight :: Int -> Row -> Row
shiftRowRight n rows = replicate n Nothing ++ rows

shiftShapeBottom :: Int -> Shape -> Shape
shiftShapeBottom n (Shape rows) = Shape (replicate  n (emptyRow (length (head rows))) ++ rows)

shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (bottom, right) shape =
    let shiftedBottom = shiftShapeBottom bottom shape
        shiftedRight = Shape (map (shiftRowRight right) (rows shiftedBottom))
    in  shiftedRight


-- ** A9
-- | padShape adds empty sqaure below and to the right of the shape
padRowRight :: Int -> Row -> Row
padRowRight n rows = rows ++ replicate n Nothing 

padShapeBottom :: Int -> Shape -> Shape
padShapeBottom n (Shape rows) = Shape (rows ++ replicate  n (emptyRow (length (head rows))))

padShape :: (Int, Int) -> Shape -> Shape
padShape (bottom, right) shape =
    let paddedBottom = padShapeBottom bottom shape
        paddedRight = Shape (map (padRowRight right) (rows paddedBottom))
    in  paddedRight

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (targetRow, targetCol) shape =
  let (numRows, numCols) = shapeSize shape 
      paddingRows = max 0 (targetRow - numRows)
      paddingCols = max 0 (targetCol - numCols)
  in padShape (paddingRows, paddingCols) shape

-- * Comparing and combining shapes

-- ** B1

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = any (uncurry rowsOverlap) (zip (rows s1) (rows s2))

rowsOverlap :: Row -> Row -> Bool
rowsOverlap row1 row2 = any (\(s1, s2) -> isJust s1 && isJust s2) (zip row1 row2)
-- ** B2
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith f shape1 shape2 = Shape (zipWith (zipWith f) (rows shape1) (rows shape2))

-- ** B3
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = let (rows1, cols1) = shapeSize s1
                      (rows2, cols2) = shapeSize s2
                      combinedRows = max rows1 rows2
                      combinedCols = max cols1 cols2
                      paddedShape1 = padShapeTo (combinedRows, combinedCols) s1
                      paddedShape2 = padShapeTo (combinedRows, combinedCols) s2
                  in zipShapeWith (\s1 s2 -> if isJust s1 then s1 else s2) paddedShape1 paddedShape2

  

{- |
Module      : Simplify
Description : Skeleton for Lab 4: simplifying polynomials.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : <Allan Khaledi, Ali Berat Can>
Lab group   : <58>
-}

module Simplify where

import Poly
import Test.QuickCheck
import Control.Monad

-- Use the following simple data type for binary operators
data BinOp = AddOp | MulOp deriving Eq

--------------------------------------------------------------------------------
-- * A1
-- Define a data type 'Expr' which represents three kinds of expression:
-- binary operators (use 'BinOp' as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable,
-- x, your data type should *not* use 'String' or 'Char' anywhere, since this is
-- not needed.

data Expr = Num Int
          | BinExpr BinOp Expr Expr
          | ExpExpr Int 
          deriving (Eq)

--------------------------------------------------------------------------------
-- * A2
-- Define the data type invariant that checks that exponents are never negative
prop_Expr :: Expr -> Bool
prop_Expr (Num _)             = True 
prop_Expr (BinExpr _ ex1 ex2) = prop_Expr ex1 && prop_Expr ex2
prop_Expr (ExpExpr n)      = n >= 0 
 

--------------------------------------------------------------------------------
-- * A3
-- Make 'Expr' an instance of 'Show' (along the lines of the example in the 
-- lecture). You can use Haskell notation for powers: x^2. You should show x^1 
-- as just x. 

instance Show Expr where
    show (Num n) = show n
    show (ExpExpr 1) = "x"
    show (ExpExpr n) = "x^" ++ show n
    show (BinExpr op e1 e2) = showExprWithOp e1 op e2

showExprWithOp :: Expr -> BinOp -> Expr -> String
showExprWithOp e1 op e2 =
    let left = addBrackets e1
        right = addBrackets e2
        opSymbol = case op of
            AddOp -> " + "
            MulOp -> " * "
    in left ++ opSymbol ++ right

addBrackets :: Expr -> String
addBrackets (BinExpr MulOp expr1 expr2) = "(" ++ show (BinExpr MulOp expr1 expr2) ++ ")"
addBrackets (BinExpr AddOp e1 e2) = addBrackets e1 ++ " + " ++ addBrackets e2
addBrackets e = show e

--------------------------------------------------------------------------------
-- * A4
-- Make 'Expr' and instance of 'Arbitrary'.
-- Now you can check the data type invariant that you defined in A2 using
-- QuickCheck.

-- (Optional)
-- Add a definition of function @shrink :: Expr -> [Expr]@ to 'Arbitrary'
-- which gives hints to QuickCheck on possible smaller expressions that it
-- could use to find a smaller counterexample for failing tests.

exprGen :: Int -> Gen Expr
exprGen size
  | size <= 0 = oneof [Num <$> arbitrary, return (ExpExpr 1)]
  | otherwise = oneof
    [ Num <$> arbitrary
    , return (ExpExpr 1)
    , BinExpr AddOp <$> subExpr <*> subExpr
    , BinExpr MulOp <$> subExpr <*> subExpr
    , ExpExpr <$> choose (2, 10)
    ]
  where
    subExpr = exprGen (size - 1)

instance Arbitrary Expr where
    arbitrary = sized exprGen


--------------------------------------------------------------------------------
-- * A5
-- Define the @eval@ function which takes a value for x and an expression and
-- evaluates it.

eval :: Int -> Expr -> Int
eval x (Num n) = n
eval x (ExpExpr n) = x ^ n
eval x (BinExpr AddOp e1 e2) = eval x e1 + eval x e2
eval x (BinExpr MulOp e1 e2) = eval x e1 * eval x e2

--------------------------------------------------------------------------------
-- * A6
-- Define @exprToPoly@ that converts an expression into a polynomial.
-- Here it is important to think recursively to just solve the bigger problem
-- by solving the smaller problems and combining them in the right way. 

exprToPoly :: Expr -> Poly
exprToPoly (Num n) = fromList [n]
exprToPoly (ExpExpr n) = fromList [if i == n then 1 else 0 | i <- [0..]]
exprToPoly (BinExpr AddOp expr1 expr2) = exprToPoly expr1 + exprToPoly expr2
exprToPoly (BinExpr MulOp expr1 expr2) = exprToPoly expr1 * exprToPoly expr2

-- Define (and check) @prop_exprToPoly@, which checks that evaluating the
-- polynomial you get from @exprToPoly@ gives the same answer as evaluating
-- the expression.

prop_exprToPoly :: Int -> Expr -> Bool
prop_exprToPoly x expr = eval x expr == evalPoly x (exprToPoly expr)

--------------------------------------------------------------------------------
-- * A7
-- Now define the function going in the other direction.

polyToExpr :: Poly -> Expr
polyToExpr poly = polyToExpr' (toList poly)
  where
    polyToExpr' :: [Int] -> Expr
    polyToExpr' [] = Num 0
    polyToExpr' [n] = Num n
    polyToExpr' (0 : cs) = polyToExpr' cs
    polyToExpr' (c : cs) = BinExpr AddOp (Num c) (powerExpr (length cs)) `addExpr` polyToExpr' cs

    -- Construct an expression for x^n
    powerExpr :: Int -> Expr
    powerExpr n
      | n == 0 = Num 1
      | n == 1 = ExpExpr 1
      | otherwise = BinExpr MulOp (ExpExpr 1) (powerExpr (n - 1))

    -- Helper function to add expressions
    addExpr :: Expr -> Expr -> Expr
    addExpr (Num a) (Num b) = Num (a + b)
    addExpr a b = BinExpr AddOp a b

-- Write (and check) a quickCheck property for this function similar to
-- question 6. 

prop_polyToExpr :: Int -> Poly -> Bool
prop_polyToExpr x poly = eval x (polyToExpr poly) == evalPoly x poly 

--------------------------------------------------------------------------------
-- * A8
-- Write a function @simplify@ which simplifies an expression by converting it 
-- to a polynomial and back again.

simplify :: Expr -> Expr
simplify = polyToExpr . exprToPoly

--------------------------------------------------------------------------------
-- * A9
-- Write a quickCheck property that checks that a simplified expression does not 
-- contain any "junk", where junk is defined to be multiplication by one or 
-- zero, addition of zero, addition or multiplication of numbers, or x to the
-- power of zero. (You may need to fix A7)

prop_noJunk :: Expr -> Bool
prop_noJunk expr = not (containsJunk expr)
    where
        containsJunk (Num 0) = True
        containsJunk (Num 1) = True
        containsJunk (ExpExpr 0) = True
        containsJunk (BinExpr AddOp e1 e2) = containsJunk e1 && containsJunk e2
        containsJunk (BinExpr MulOp e1 e2) = containsJunk e1 && containsJunk e2
        containsJunk _ = False

--------------------------------------------------------------------------------
-- * A10
-- Write two IO functions that read respectively write the difficulty, which is
-- modelled as a natural number. Use the 'diffFile' as file path. Note that the
-- difficulty should never be below zero.

type Difficulty = Int

diffFile :: FilePath
diffFile = "difficulty.txt"

readDifficulty :: IO Difficulty
readDifficulty = do
  content <- readFile diffFile
  return (read content)

writeDifficulty :: Difficulty -> IO ()
writeDifficulty difficulty
  | difficulty < 1 = putStrLn "Difficulty cannot be negative. Setting to 0." >> writeFile diffFile "0"
  | otherwise = writeFile diffFile (show difficulty)
--------------------------------------------------------------------------------
-- * A11
-- Define the 'play' function that generates a random expression, a random 
-- value for @x@, show the simplified expression and ask the user to solve it. 
-- If the guess is as expected, give a nice feedback message and increase the 
-- difficulty by one. If the guess was wrong, again give feedback and decrease 
-- the difficulty by one. Then play again.

play :: IO ()
play = do
  difficulty <- readDifficulty
  expr <- generateRandomExpr difficulty 
  xValue <- generateRandomXValue
  let simplifiedExpr = simplify expr
  putStrLn $ "Simplify the following expression with x = " ++ show xValue
  putStrLn $ show expr
  userGuess <- readLn
  if userGuess == eval xValue simplifiedExpr
    then do
      putStrLn "Well done!"
      writeDifficulty (difficulty + 1)
      play
    else do
      putStrLn $ "No, it should have been " ++ show (eval xValue simplifiedExpr)
      writeDifficulty (difficulty - 1)
      play

generateRandomExpr :: Difficulty -> IO Expr
generateRandomExpr difficulty = generate (resize difficulty arbitrary)

generateRandomXValue :: IO Int
generateRandomXValue = generate arbitrary

--------------------------------------------------------------------------------

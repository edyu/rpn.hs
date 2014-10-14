{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Exception (catch)

type Stack = [Double]
type Error = String
type Result = ([Error], Stack)

type UnaOp = (Double -> Double)
type BinOp = (Double -> Double -> Double)
type ArrOp = ([Double] -> Double)

class Calculable a where
    calculate :: Stack -> a -> Result

instance Calculable UnaOp where
    calculate [] _     = ([errOp], [])
    calculate (x:xs) f = ([], f x : xs)

instance Calculable BinOp where
    calculate [] _       = ([errOp], [])
    calculate (x:y:xs) f = ([], (y `f` x) : xs)
    calculate xs@(_:_) _ = ([errOp], xs)

instance Calculable ArrOp where
    calculate xs f = ([], [(f xs)])

errOp :: Error
errOp = "not enough operands"
errDiv :: Error
errDiv = "division by zero"
errNum :: Error
errNum = "can't parse number"

main :: IO ()
main = do
    processInput []

catchEOF :: IO a -> (IOError -> IO a) -> IO a
catchEOF = Control.Exception.catch

processInput :: Stack -> IO ()
processInput stack = do
    line <- catchEOF getLine (\_ -> return "q")
    case line of
        "q" -> do
            putStrLn "goodbye!"
            return ()
        "?" -> do
            print stack
            processInput stack
        _ -> do
            let (errors, newStack) = process ([], stack) $ tokenize line
            if null errors
              then print $ head newStack
              else printErrors errors
            processInput newStack
            where printErrors = mapM_ putStrLn
                  tokenize    = words

process :: Result -> [String] -> Result
process stacks []                 = stacks
process stacks ("+":xs)           = calcStack stacks ((+) :: BinOp) xs
process stacks ("-":xs)           = calcStack stacks ((-) :: BinOp) xs
process stacks ("*":xs)           = calcStack stacks ((*) :: BinOp) xs
process (errs, s@(0:_)) ("/":xs)  = process (errDiv : errs, s) xs
process (errs, s@(0:_)) ("%":xs)  = process (errDiv : errs, s) xs
process stacks ("/":xs)           = calcStack stacks ((/) :: BinOp) xs
process stacks ("%":xs)           = calcStack stacks ((%) :: BinOp) xs
process stacks ("^":xs)           = calcStack stacks ((**) :: BinOp) xs
process stacks ("!":xs)           = calcStack stacks (gamma :: UnaOp) xs
process stacks ("sum":xs)         = calcStack stacks (sum :: ArrOp) xs
process (errs, stack) (x:xs)      = case parseNumber x of
                                    ([e], []) -> process (e : errs, stack) xs
                                    ([], [n]) -> process (errs, n : stack) xs
                                    _         -> process (errs, stack) xs
    where parseNumber y = case reads y of
                          []        -> ([errNum], [])
                          [(n, _)]  -> ([], [n])
                          _         -> ([], [])

calcStack :: Calculable a => Result -> a -> [String] -> Result
calcStack (errs, stack) op = let (e, s) = calculate stack op
                             in  process (e ++ errs, s)

(%) :: BinOp
(%) x y = fromIntegral ((toInt x) `mod` (toInt y))
    where toInt :: Double -> Int
          toInt = round

gamma :: UnaOp
gamma x = fromIntegral (factorial (toInt x))
    where factorial y | y <= 0 = 1
                      | otherwise = y * factorial (y - 1)
          toInt :: Double -> Int
          toInt = floor

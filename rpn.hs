{-# OPTIONS_GHC -Wall #-}

type Stack = [Double]
type Error = String
type Result = ([Error], Stack)
type BinOp = (Double -> Double -> Double)
type ArrOp = ([Double] -> Double)
type UnaOp = (Double -> Double)

errOp :: Error
errOp = "not enough operands"
errDiv :: Error
errDiv = "division by zero"
errNum :: Error
errNum = "can't parse number"

main :: IO ()
main = do
  processInput []

processInput :: Stack -> IO ()
processInput stack = do
  line <- getLine
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
      where printErrors [] = return ()
            printErrors (x:xs) = do
              putStrLn x
              printErrors xs
            tokenize = words

process :: Result -> [String] -> Result
process stacks [] = stacks
process stacks ("+":xs) = processBinOp stacks (+) xs
process stacks ("-":xs) = processBinOp stacks (-) xs
process stacks ("*":xs) = processBinOp stacks (*) xs
process (errs, s@(0:_)) ("/":xs) = process (errDiv : errs, s) xs
process (errs, s@(0:_)) ("%":xs) = process (errDiv : errs, s) xs
process stacks ("/":xs) = processBinOp stacks (/) xs
process stacks ("%":xs) = processBinOp stacks (%) xs
process stacks ("^":xs) = processBinOp stacks (**) xs
process stacks ("!":xs) = processUnaOp stacks gamma xs
process stacks ("sum":xs) = processArrOp stacks sum xs
process (err, stack) (x:xs) = case parseNumber x of
                                ([e], []) -> process (e : err, stack) xs
                                ([], [n]) -> process (err, n : stack) xs
                                _ -> process (err, stack) xs
  where parseNumber y = case reads y of
          [] -> ([errNum], [])
          [(n, _)] -> ([], [n])
          _ -> ([], [])

processBinOp :: Result -> BinOp -> [String] -> Result
processBinOp (errs, stack) op = let (newErr, newStack) = binaryMath stack op
                                in  process (newErr ++ errs, newStack)
  where binaryMath [] _ = ([errOp], [])
        binaryMath (x:y:xs) f = ([], (y `f` x) : xs)
        binaryMath xs@(_:_) _ = ([errOp], xs)

processUnaOp :: Result -> UnaOp -> [String] -> Result
processUnaOp (errs, stack) op = let (newErr, newStack) = unaryMath stack op
                                in process (newErr ++ errs, newStack)
  where unaryMath [] _ = ([errOp], [])
        unaryMath (x:xs) f = ([], f x : xs)

processArrOp :: Result -> ArrOp -> [String] -> Result
processArrOp (errs, stack) op = let (newErr, newStack) = arrayMath stack op
                                in process (newErr ++ errs, newStack)
  where arrayMath xs f = ([], [(f xs)])

(%) :: Double -> Double -> Double
(%) x y = fromIntegral ((toInt x) `mod` (toInt y))
  where toInt :: Double -> Int
        toInt = round

gamma :: Double -> Double
gamma x = fromIntegral (factorial (toInt x))
  where factorial y | y <= 0 = 1
                    | otherwise = y * factorial (y - 1)
        toInt :: Double -> Int
        toInt = floor

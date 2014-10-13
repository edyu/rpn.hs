{-# OPTIONS_GHC -Wall #-}

type Stack = [Double]
type Error = String
type Result = ([Error], Stack)
type BinOp = (Double -> Double -> Double)

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
process (errs, stack) [] = (errs, stack)
process stacks ("+":xs) = doMath stacks (+) xs
process stacks ("-":xs) = doMath stacks (-) xs
process stacks ("*":xs) = doMath stacks (*) xs
process (errs, s@(0:_)) ("/":xs) = process (errDiv : errs, s) xs
process stacks ("/":xs) = doMath stacks (/) xs
process (err, stack) (x:xs) = case parseNumber x of
                                ([e], []) -> process (e : err, stack) xs
                                ([], [n]) -> process (err, n : stack) xs
                                _ -> process (err, stack) xs
  where parseNumber y = case reads y of
          [] -> ([errNum], [])
          [(n, _)] -> ([], [n])
          _ -> ([], [])

doMath :: ([Error], Stack) -> BinOp -> [String] -> Result
doMath (errs, stack) op = let (newErr, newStack) = binaryMath stack op
                          in  process (newErr ++ errs, newStack)

binaryMath :: Stack -> BinOp -> Result
binaryMath [] _ = ([errOp], [])
binaryMath (x:y:xs) op = ([], (y `op` x) : xs)
binaryMath xs@(_:_) _ = ([errOp], xs)

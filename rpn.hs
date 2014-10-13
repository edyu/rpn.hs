{-# OPTIONS_GHC -Wall #-}

type Stack = [Double]
type Error = String
type Result = ([Error], Stack)

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
  if line == "q"
    then do
      putStrLn "goodbye!"
      return ()
    else do
      let (err, result) = process ([], stack) $ tokenize line
      if null err
        then print $ head result
        else printErrors err
      processInput result

printErrors :: [Error] -> IO ()
printErrors [] = return ()
printErrors (x:xs) = do
                       putStrLn x
                       printErrors xs

tokenize :: String -> [String]
tokenize = words

doMath :: ([Error], t) -> (t -> ([Error], Stack)) -> [String] -> Result
doMath (errs, stack) math = let res = math stack
                            in  process (fst res ++ errs, snd res)

process :: Result -> [String] -> Result
process (errs, stack) [] = (errs, stack)
process stacks ("+":xs) = doMath stacks add2 xs
process stacks ("-":xs) = doMath stacks sub2 xs
process stacks ("*":xs) = doMath stacks mul2 xs
process stacks ("/":xs) = doMath stacks div2 xs
process (err, stack) (x:xs) = case parseNumber x of
                                ([e], []) -> process (e : err, stack) xs
                                ([], [n]) -> process (err, n : stack) xs
                                _ -> process (err, stack) xs

parseNumber :: String -> Result
parseNumber x = case res of
                  [] -> ([errNum], [])
                  [(n, _)] -> ([], [n])
                  _ -> ([], [])
  where res = reads x

add2 :: [Double] -> Result
add2 [] = ([errOp], [])
add2 (x:y:xs) = ([], (y + x) : xs)
add2 xs@(_:_) = ([errOp], xs)

sub2 :: [Double] -> Result
sub2 [] = ([errOp], [])
sub2 (x:y:xs) = ([], (y - x) : xs)
sub2 xs@(_:_) = ([errOp], xs)

mul2 :: [Double] -> Result
mul2 [] = ([errOp], [])
mul2 (x:y:xs) = ([], (y * x) : xs)
mul2 xs@(_:_) = ([errOp], xs)

div2 :: [Double] -> Result
div2 [] = ([errOp], [])
div2 xs@(0:_) = ([errDiv], xs)
div2 (x:y:xs) = ([], (y / x) : xs)
div2 xs@(_:_) = ([errOp], xs)

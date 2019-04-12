{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Digits

biggie :: Integer
biggie = 277777788888899

mDecimalDigitsRev :: Integer -> Maybe [Integer]
mDecimalDigitsRev = Data.Digits.mDigitsRev 10

persistence :: Integer -> Integer
persistence input | input <= 10 = 0
persistence input =
  case mDecimalDigitsRev input of
    Just xs@(_ : _ : _) ->
      let p = product xs
      in
        if p <= 9
          then 1
          else 1 + persistence p
    _ -> 0

persistenceList :: [Integer] -> Integer
persistenceList [] = 0
persistenceList (_ : []) = 0
persistenceList digits@(_ : _ : _) =
  let p = product digits
  in 1 + persistence p

persistenceIO :: Integer -> IO Integer
persistenceIO input | input <= 10 = return 0
persistenceIO input =
  case mDecimalDigitsRev input of
    Just xs@(_ : _ : _) -> do
      let p = product xs
      print p
      if p <= 9
        then return 1
        else do
          r <- persistenceIO p
          return (1 + r)
    _ -> return 0

persistenceListIO :: [Integer] -> IO Integer
persistenceListIO [] = return 0
persistenceListIO (_ : []) = return 0
persistenceListIO digits@(_ : _ : _) = do
  let p = product digits
  print p
  r <- persistenceIO p
  return (r + 1)

digitGenerator :: Integer -> Int -> [[Integer]]
digitGenerator d maxDigits = [replicate x d | x <- [0..maxDigits]]

digitNumberGenerator :: Int -> [[Integer]]
digitNumberGenerator maxDigits =
  [d2++d3++d4++d5++d7++d8++d9
  | d2 <- digitGenerator 2 1
  , d3 <- digitGenerator 3 1
  , d4 <- digitGenerator 4 1
  , d5 <- digitGenerator 5 maxDigits
  , d7 <- digitGenerator 7 maxDigits
  , d8 <- digitGenerator 8 maxDigits
  , d9 <- digitGenerator 9 maxDigits
  , (null d2 && null d4 && null d8) || null d5
  , not (not (null d2) && not (null d4))
  ]

loop :: [Integer] -> IO ()
loop [] = print "Ran out of numbers"
loop (n : ns) = do
  print n
  let p = persistence n
  if p >= 3
    then print (n, p)
    else return ()
  loop ns

loopList :: [[Integer]] -> IO ()
loopList [] = print "Ran out of numbers"
loopList (n : ns) = do
  let p = persistenceList n
  if p >= 10
    then print (concat (fmap show n), p)
    else return ()
  loopList ns

main :: IO ()
main = loopList $ digitNumberGenerator 20
-- main = print $ length $ digitNumberGenerator 40
-- main = mapM_ print $ take 1000 $ digitNumberGenerator 3

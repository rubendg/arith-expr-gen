{-# LANGUAGE MultiParamTypeClasses, RankNTypes #-}
module Gen where

import Core
import Common
import Control.Applicative
import Control.Monad.Random
import Data.List (sortBy)
import Data.Ratio

type Dist = Rand StdGen

data Config = Config 
  { maxNumOps          :: Int
  , integerPart    :: Dist Int
  , fractionalPart :: Dist Int
  , opProb   :: Rational
  , operations :: forall a r. Dist (r -> r -> ExprF r)
  , primitives :: Int -> Int -> Dist Expr
  }

uniform :: [a] -> Dist a
uniform xs = do
   ix <- getRandomR (0, length xs - 1)
   return (xs !! ix)

nonuniform :: [(a, Rational)] -> Dist a
nonuniform [] = error "empty list"
nonuniform xs = do
  let total = totalProbability xs
      size  = length xs - 1
      xs'   = sortBy (\a b -> snd a `compare` snd b) xs

  r <- getRandomR (0, fromRational total) :: Dist Double

  let loop xs n i 
          | i < size = 
              let indexed = xs !! i
                  n' = n + (fromRational $ snd indexed)
              in  if n' >= r
                    then indexed
                    else loop xs n' (i + 1)
          | otherwise = xs !! i

  return (fst $ loop xs' 0 0)

totalProbability :: [(a, Rational)] -> Rational
totalProbability = sum . map snd

generateExpression :: Config -> Dist Expr
generateExpression config = 
  let 
    bf = opProb config

    selectPrim = do 
      i <- integerPart config 
      f <- fractionalPart config
      primitives config i f

    root maxNumOps 
      | maxNumOps <= 0 = selectPrim
      | otherwise = do
          op <- operations config
          (\a b -> Fx $ op a b) <$> children maxNumOps <*> children (maxNumOps - 1)

    children maxNumOps
      | maxNumOps <= 0 = selectPrim
      | otherwise  = 
        (nonuniform $ [(selectPrim, 1 - bf), (root (maxNumOps - 1), bf)]) >>= id

  in root (maxNumOps config)
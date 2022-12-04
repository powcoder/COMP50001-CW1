https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module AutoTest where

import Submission1 hiding (maximum)
import Test (test, testFromFile, runTests, Test (..), Question(..))

import Data.Map (fromList)
import qualified Data.Map as M
import Data.List (intercalate)
import Data.Coerce
import Control.Exception
import GHC.Generics
import Data.Type.Bool
import Data.List
import Data.Function
import Data.Ord (comparing, Down (..))

import Control.Monad
import Control.DeepSeq

import System.IO


-- | The Marks per question
qn :: Int -> Question
qn i 
    | i == 10 = Question ("10", 10)
    | otherwise = Question ("0" ++ show i, 10)

main :: IO ()
main = runTests $ do
  testFromFile "Knapsack' tests" (qn 1) (uncurry (knapsack' @String @Int @Int)) (const (==)) "tests/knapsackTests.txt"
  testFromFile "Knapsack'' tests" (qn 2) (uncurry (knapsack'' @String @Int @Int)) (checkKnapsack True) "tests/knapsackTests2.txt"
  testFromFile "Bounded knapsack tests" (qn 3) (uncurry (bknapsack @String @Int @Int)) (checkKnapsack False) "tests/bknapsackTests.txt"
  testFromFile "Bounded knapsack' tests" (qn 4) (uncurry3 (bknapsack' @String @Int @Int)) checkKnapsack' "tests/bknapsackTests2.txt"
  testFromFile "Bounded knapsack'' tests" (qn 5) (uncurry (bknapsack'' @String @Int @Int)) (checkKnapsack False) "tests/bknapsackTests3.txt"
  testFromFile "Dijkstra tests" (qn 6) (uncurry (shortestPaths @[(String, String, Integer)])) (const cmpShortestPaths) "tests/dijkstra.txt"
  testFromFile "Dijkstra (heap) tests" (qn 7) (uncurry (shortestPaths' @[(String, String, Integer)])) (const cmpShortestPaths) "tests/dijkstra.txt"
  testFromFile "Heap insert tests" (qn 8) (fromPQueue . toPQueue @Heap @Int compare) (const (==)) "tests/insert.txt"
  testFromFile "Heap rank tests" (qn 8) (rankHeap . toPQueue @Heap @Int compare) (const (==)) "tests/rank.txt"
  test "AdjList vertices tests" (qn 9) vertices (const ((==) `on` sort)) [adjList :=> ["a", "b", "c", "d"]]
  test "AdjList edges tests" (qn 9) edges (const ((==) `on` sort)) [adjList :=> [("a","b",10),("a","c",20),("b","a",5),("b","d",8),("d","b",3),("d","a",4)]]
  test "AdjList edgesFrom tests" (qn 9) (uncurry edgesFrom) (const ((==) `on` sort)) [(adjList, "a") :=> [("a","b",10),("a","c",20)], (adjList, "b") :=> [("b","a",5),("b","d",8)]]
  test "AdjList edgesTo tests" (qn 9) (uncurry edgesTo) (const ((==) `on` sort)) [(adjList, "a") :=> [("b","a",5),("d","a",4)], (adjList, "b") :=> [("a","b",10),("d","b",3)]]
  test "AdjList velem tests" (qn 9) (uncurry velem) (const (==)) [("x", adjList) :=> False, ("a", adjList) :=> True]
  test "AdjList eelem tests" (qn 9) (uncurry eelem) (const (==)) [(("a", "b", 3), adjList) :=> False, (("b", "d", 8), adjList) :=> True]
  testFromFile "Conflict zones tests" (qn 10) (uncurry3 conflictZones) (const ((==) `on` (\(a, b, c) -> (sort a, sort b, sort c)))) "tests/conflictZonesTests.txt"

uncurry3 :: (a->b->c -> d) -> (a,b,c) -> d
uncurry3 f ~(a, b, c) = f a b c

instance Show (String -> String -> Ordering) where
  show _ = "<<function>>"

-- To check a solution of the Knapsack problem, we verify its weight is equal to the standard
-- answer and that it is indeed a valid selection.
checkKnapsack :: Bool -> ([(String, Int, Int)], Int) -> (Int, [String]) -> (Int, [String]) -> Bool
checkKnapsack unbounded (items, w) (expVal, _) (actVal, actItems) = expVal == actVal && validSelection 
  where
    itemsMap :: M.Map String (Int, Int)
    itemsMap = fromList (map (\(n, w, v) -> (n, (w, v))) items)

    validSelection :: Bool
    validSelection = correctWV && (unbounded || distinctItems)

    correctWV :: Bool
    correctWV = case sequence [ M.lookup n itemsMap | n <- actItems ] of
                       Nothing    -> False
                       (Just wvs) -> sum (map fst wvs) <= w && sum (map snd wvs) == actVal

    distinctItems :: Bool
    distinctItems = all ((==) 1 . length) (group (sort actItems))

-- The following is a special version for checking knapsack'.
checkKnapsack' :: ([(String, Int, Int)], Int, Int) -> (Int, [String]) -> (Int, [String]) -> Bool
checkKnapsack' = checkKnapsack False . (\(a,b,c) -> (a,c))

adjList :: AdjList (String, String, Integer) String
adjList
  = AdjList
      [ ("a", [("a", "b", 10), ("a", "c", 20)])
      , ("b", [("b", "a", 5), ("b", "d", 8)])
      , ("c", [])
      , ("d", [("d", "b", 3), ("d", "a", 4)])
      ]

cmpShortestPaths :: Edge e v => [Path e] -> [Path e] -> Bool
cmpShortestPaths qs ps = contains ps qs && contains qs ps where
  contains qs ps = all (\p -> not (null (filter (sameAs p) qs))) ps
  
  sameAs q p = (source p == source q)
                  && (target p == target q) 
                  && (weight p == weight q)

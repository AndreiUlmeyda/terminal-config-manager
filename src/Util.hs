module Util (changeNthElement, changeNthElement', elementAfter, elementBefore) where

import Data.List.NonEmpty (NonEmpty, fromList, toList)

data NeighborSelection = SelectSuccessor | SelectPredecessor

elementNextTo :: Eq p => NeighborSelection -> p -> [p] -> p
elementNextTo neighborSelection targetItem list
  | null list = targetItem
  | otherwise = (head . tail . dropWhile (/= targetItem) . cycledList) list
  where
    cycledList :: [a] -> [a]
    cycledList = case neighborSelection of
      SelectSuccessor -> cycle
      SelectPredecessor -> tail . cycle . reverse

elementAfter :: Eq a => a -> [a] -> a
elementAfter = elementNextTo SelectSuccessor

elementBefore :: Eq a => a -> [a] -> a
elementBefore = elementNextTo SelectPredecessor

changeNthElement :: Int -> (a -> a) -> NonEmpty a -> NonEmpty a
changeNthElement n fn = fromList . changeNthElement' n fn . toList

changeNthElement' :: Int -> (a -> a) -> [a] -> [a]
changeNthElement' _ _ [] = []
changeNthElement' n fn (x : xs)
  | (n < 0) = x : xs
  | (n == 0) = (fn x) : xs
  | otherwise = x : (changeNthElement' (n - 1) fn xs)
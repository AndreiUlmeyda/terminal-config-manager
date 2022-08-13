module Infrastructure.Util
  ( changeNthElement,
    changeNthElementNonEmpty,
    elementAfter,
    elementBefore,
  )
where

import Data.List.NonEmpty
  ( NonEmpty,
    fromList,
    toList,
  )

data NeighborSelection = SelectSuccessor | SelectPredecessor

-- | Finds the first element equal to the input element inside of a list and returns an element next to it depending on the supplied selection policy.
elementNextTo :: Eq t => NeighborSelection -> t -> [t] -> t
elementNextTo neighborSelection targetItem list
  | null list = targetItem
  | otherwise = (head . tail . dropWhile (/= targetItem) . cycledList) list
  where
    cycledList :: [t] -> [t]
    cycledList = case neighborSelection of
      SelectSuccessor -> cycle
      SelectPredecessor -> tail . cycle . reverse

-- |  Finds the first element equal to the input element inside of a list and returns the element after it (one index up).
elementAfter :: Eq t => t -> [t] -> t
elementAfter = elementNextTo SelectSuccessor

-- |  Finds the first element equal to the input element inside of a list and returns the element before it (one index down).
elementBefore :: Eq t => t -> [t] -> t
elementBefore = elementNextTo SelectPredecessor

-- |  Given an index, a function and a NonEmpty list it applies the function to the element at that index.
changeNthElementNonEmpty :: Int -> (t -> t) -> NonEmpty t -> NonEmpty t
changeNthElementNonEmpty n fn = fromList . changeNthElement n fn . toList

-- |  Given an index, a function and a list it applies the function to the element at that index.
changeNthElement :: Int -> (t -> t) -> [t] -> [t]
changeNthElement _ _ [] = []
changeNthElement n fn (x : xs)
  | (n < 0) = x : xs
  | (n == 0) = (fn x) : xs
  | otherwise = x : (changeNthElement (n - 1) fn xs)
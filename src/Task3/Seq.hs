{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.Seq where

import Common.Sequence ( Sequence(..) )
import Common.MonoidalTree ( MonoidalTree((|>), (<|)) )

import Task1 (Measured(..), Size(..))
import Task3.Tree
    ( InsertResult(..),
      RemoveResult(..),
      Tree(..),
      leaf,
      node2,
      node3,
      balance2Left,
      balance2Right,
      balance3Left,
      balance3Middle,
      balance3Right )

-- * Sequence definition

-- | Random-access sequence based on binary tree
newtype Seq a = Seq { getTree :: Tree (Size a) (Elem a) }
  deriving (Show, Eq)

-- | Sequence element wrapper
newtype Elem a = Elem { getElem :: a }
  deriving (Show, Eq)

-- | Measures given element as 'Size 1'
instance Measured (Size a) (Elem a) where
  measure _ = Size 1

instance Foldable Seq where
  foldMap f = foldMap (f . getElem) . getTree

  -- An O(1) implementation of length is possible
  -- due to size of the tree being cached at each node
  length :: forall a. Seq a -> Int
  length (Seq t) = getSize (measure t :: Size a)

-- * Sequence instance

instance Sequence Seq where
  empty = Seq Empty

  toSequence = foldl (|+) empty

  x +| (Seq t) = Seq (Elem x <| t)

  Seq t |+ x = Seq (t |> Elem x)

  insertAt pos val (Seq tree) =
    let idx = max 0 (min pos (length (Seq tree)))
    in case step idx (Elem val) tree of
         Done rest -> Seq rest
         Split l r -> Seq (node2 l r)
    where
      step _ y Empty = Done (leaf y)
      step 0 y (Leaf z) = Done (node2 (leaf y) (leaf z))
      step 1 y (Leaf z) = Done (node2 (leaf z) (leaf y))
      step _ _ (Leaf _) = error "invalid index for Leaf"
      step i y (Node2 _ a b)
        | i <= szA = case step i y a of
            Done a' -> Done (node2 a' b)
            Split u v -> Done (node3 u v b)
        | otherwise = case step (i - szA) y b of
            Done b' -> Done (node2 a b')
            Split u v -> Done (node3 a u v)
        where szA = getLen a
      step i y (Node3 _ a b c)
        | i <= szA = case step i y a of
            Done a' -> Done (node3 a' b c)
            Split u v -> Split (node2 u v) (node2 b c)
        | i <= szA + szB = case step (i - szA) y b of
            Done b' -> Done (node3 a b' c)
            Split u v -> Split (node2 a u) (node2 v c)
        | otherwise = case step (i - szA - szB) y c of
            Done c' -> Done (node3 a b c')
            Split u v -> Split (node2 a b) (node2 u v)
        where
          szA = getLen a
          szB = getLen b

  removeAt idx (Seq tree)
    | idx < 0 || idx >= getLen tree = Seq tree
    | otherwise = case del idx tree of
        Balanced t -> Seq t
        Hole t -> Seq t
    where
      del _ Empty = Balanced Empty
      del _ (Leaf _) = Hole Empty
      del i (Node2 _ a b)
        | i < szA = case del i a of
            Balanced a' -> Balanced (node2 a' b)
            Hole a' -> balance2Left a' b
        | otherwise = case del (i - szA) b of
            Balanced b' -> Balanced (node2 a b')
            Hole b' -> balance2Right a b'
        where szA = getLen a
      del i (Node3 _ a b c)
        | i < szA = case del i a of
            Balanced a' -> Balanced (node3 a' b c)
            Hole a' -> balance3Left a' b c
        | i < szA + szB = case del (i - szA) b of
            Balanced b' -> Balanced (node3 a b' c)
            Hole b' -> balance3Middle a b' c
        | otherwise = case del (i - szA - szB) c of
            Balanced c' -> Balanced (node3 a b c')
            Hole c' -> balance3Right a b c'
        where
          szA = getLen a
          szB = getLen b

  elemAt pos (Seq tree) = case tree of
    Empty -> Nothing
    Leaf x
      | pos == 0 -> Just (getElem x)
      | otherwise -> Nothing
    Node2 _ a b
      | pos < szA -> elemAt pos (Seq a)
      | otherwise -> elemAt (pos - szA) (Seq b)
      where szA = getLen a
    Node3 _ a b c
      | pos < szA -> elemAt pos (Seq a)
      | pos < szA + szB -> elemAt (pos - szA) (Seq b)
      | otherwise -> elemAt (pos - szA - szB) (Seq c)
      where
        szA = getLen a
        szB = getLen b

getLen :: forall a. Tree (Size a) (Elem a) -> Int
getLen t = getSize (measure t :: Size a)

{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3.Tree where

import Common.MonoidalTree ( MonoidalTree(..) )

import Task1 (Measured(..))

-- * 2-3 tree definition

-- | 2-3 tree with values 'a' in leaves
-- Intermediate nodes contain only accumulated measure 'm'
data Tree m a
  = Empty
  | Leaf a
  | Node2 m (Tree m a) (Tree m a)
  | Node3 m (Tree m a) (Tree m a) (Tree m a)
  deriving (Show, Eq)

-- | Measures given tree using provided measure of 'a'
instance Measured m a => Measured m (Tree m a) where
  measure Empty           = mempty
  measure (Leaf x)        = measure x
  measure (Node2 m _ _)   = m
  measure (Node3 m _ _ _) = m

instance Foldable (Tree m) where
  foldMap _ Empty          = mempty
  foldMap f (Leaf x)       = f x
  foldMap f (Node2 _ l r)  = foldMap f l <> foldMap f r
  foldMap f (Node3 _ l m r) = foldMap f l <> foldMap f m <> foldMap f r

-- * Smart constructors

leaf :: a -> Tree m a
leaf = Leaf

node2 :: Measured m a => Tree m a -> Tree m a -> Tree m a
node2 l r = Node2 (measure l <> measure r) l r

node3 :: Measured m a => Tree m a -> Tree m a -> Tree m a -> Tree m a
node3 l m r = Node3 (measure l <> measure m <> measure r) l m r

-- * Monoidal tree instance

data InsertResult m a
  = Done (Tree m a)
  | Split (Tree m a) (Tree m a)

data RemoveResult m a
  = Balanced (Tree m a)
  | Hole (Tree m a)

instance MonoidalTree Tree where
  toTree = foldl (|>) Empty
  
  x <| t = case insertLeft x t of
    Done t' -> t'
    Split l r -> node2 l r

  t |> x = case insertRight x t of
    Done t' -> t'
    Split l r -> node2 l r

insertLeft :: Measured m a => a -> Tree m a -> InsertResult m a
insertLeft x Empty = Done (leaf x)
insertLeft x (Leaf y) = Split (leaf x) (leaf y)
insertLeft x (Node2 _ l r) = case insertLeft x l of
  Done l' -> Done (node2 l' r)
  Split l' r' -> Done (node3 l' r' r)
insertLeft x (Node3 _ l m r) = case insertLeft x l of
  Done l' -> Done (node3 l' m r)
  Split l' r' -> Split (node2 l' r') (node2 m r)

insertRight :: Measured m a => a -> Tree m a -> InsertResult m a
insertRight x Empty = Done (leaf x)
insertRight x (Leaf y) = Split (leaf y) (leaf x)
insertRight x (Node2 _ l r) = case insertRight x r of
  Done r' -> Done (node2 l r')
  Split l' r' -> Done (node3 l l' r')
insertRight x (Node3 _ l m r) = case insertRight x r of
  Done r' -> Done (node3 l m r')
  Split l' r' -> Split (node2 l m) (node2 l' r')

balance2Left :: Measured m a => Tree m a -> Tree m a -> RemoveResult m a
balance2Left l' r = case r of
  Empty -> Hole l'
  Leaf x -> Hole (leaf x)
  Node2 _ rl rr -> Hole (node3 l' rl rr)
  Node3 _ rl rm rr -> Balanced (node2 (node2 l' rl) (node2 rm rr))

balance2Right :: Measured m a => Tree m a -> Tree m a -> RemoveResult m a
balance2Right l r' = case l of
  Empty -> Hole r'
  Leaf x -> Hole (leaf x)
  Node2 _ ll lr -> Hole (node3 ll lr r')
  Node3 _ ll lm lr -> Balanced (node2 (node2 ll lm) (node2 lr r'))

balance3Left :: Measured m a => Tree m a -> Tree m a -> Tree m a -> RemoveResult m a
balance3Left l' m r = case m of
  Empty -> Balanced r
  Leaf x -> Balanced (node2 (leaf x) r)
  Node2 _ ml mr -> Balanced (node2 (node2 l' ml) (node2 mr r))
  Node3 _ ml mm mr -> Balanced (node3 (node2 l' ml) (node2 mm mr) r)

balance3Middle :: Measured m a => Tree m a -> Tree m a -> Tree m a -> RemoveResult m a
balance3Middle l m' r = case l of
  Empty -> Balanced r
  Leaf x -> Balanced (node2 (leaf x) r)
  Node2 _ ll lr -> Balanced (node2 (node2 ll lr) (node2 m' r))
  Node3 _ ll lm lr -> Balanced (node3 (node2 ll lm) (node2 lr m') r)

balance3Right :: Measured m a => Tree m a -> Tree m a -> Tree m a -> RemoveResult m a
balance3Right l m r' = case m of
  Empty -> Balanced l
  Leaf x -> Balanced (node2 l (leaf x))
  Node2 _ ml mr -> Balanced (node2 (node2 l ml) (node2 mr r'))
  Node3 _ ml mm mr -> Balanced (node3 l (node2 ml mm) (node2 mr r'))

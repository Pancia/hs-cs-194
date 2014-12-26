{- BST.hs
   Part of an implementation of a binary search tree.
-}

module BST where

import Test.QuickCheck
import Control.Applicative

data BST a = Leaf
           | Node (BST a) a (BST a)
        deriving (Show)

-- | Is the tree empty?
isEmpty :: BST a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Get a list of the elements in sorted order
getElements :: BST a -> [a]
getElements Leaf                = []
getElements (Node left x right) = getElements left ++ x : getElements right

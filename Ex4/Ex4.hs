import BST

insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST cmp i (Leaf) = Node (Leaf) i (Leaf)
insertBST cmp i bst@(Node l a r)
        | (cmp i a) == LT = Node (insertBST cmp i l) a r
        | (cmp i a) == GT = Node l a (insertBST cmp i r)
        | otherwise = bst

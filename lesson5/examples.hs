data Tree a = Leaf a
    | Branch (Tree a) (Tree a)
        deriving (Eq, Show, Ord)

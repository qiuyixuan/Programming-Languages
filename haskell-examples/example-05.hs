-- Examples of functors
-- List, Maybe (built in to Haskell)
-- Trees (we defined here)
-- 

-- new type: a Tree is either a leaf or a left and right branch
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show
-- Trees are functors
instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

main :: IO ()
-- showing off different kinds of maps!
main = let treeMap = fmap (\n -> n / 6) (Branch (Branch (Leaf 6) (Leaf 30)) (Leaf 12)) in
       let listMap = fmap (1+) [0, 4, 1] in
       let maybeMap = fmap (++ " 152") (Just "CS") in
        do 
            print treeMap  -- what should this print? 
            print listMap  -- what should this print?
            print maybeMap -- what should this print?

-- Hmm... we know List and Maybe are monads as well as functors...
-- How can we define fmap in terms of the monad operators?
-- Hint: (.) denotes function composition
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- Examples of functors
-- List, Maybe (built in to Haskell)
-- Trees
-- 
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show
instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

-- You can define fmap in terms of the monad operations:
--                fmap f xs = xs >>= return . f
-- where (.) is function composition
-- fmap can also be written with infix notation as "<$>"

main :: IO ()
-- showing off different kinds of maps
main = let treeMap = fmap (\n -> n / 6) (Branch (Branch (Leaf 6) (Leaf 30)) (Leaf 12)) in
       let listMap = fmap (1+) [0, 4, 1] in
       let maybeMap1 = fmap (++ " 152") (Just "CS") in
       -- showing off our different ways to implement fmap
       let maybeMap2 = (++ " 152") <$> (Just "CS") in 
       let maybeMonadMap =  (Just "CS") >>= (return . (++ " 152")) in
        do 
            print treeMap 
            print listMap
            print maybeMap1
            print maybeMap2
            print maybeMonadMap

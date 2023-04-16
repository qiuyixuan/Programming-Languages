-- Breeding sheep without monads
-- Exercise from wiki.haskell.org/All_About_Monads

-- defines a new type "Sheep"
data Sheep = Sheep {name :: String, mother :: Maybe Sheep, father :: Maybe Sheep}

-- states that "Sheep" is an instance of "Show"
-- must implement a "show" method
instance Show Sheep where
    show s = show (name s)

-- the Sheep family tree
breedSheep :: Sheep
breedSheep = let adam = Sheep "Adam" Nothing Nothing
                 eve  = Sheep "Eve" Nothing Nothing
                 cain = Sheep "Cain" (Just eve) (Just adam)
                 abel = Sheep "Abel" (Just eve) (Just adam)
                 eru = Sheep "Eru" Nothing Nothing 
                 manwe = Sheep "Manwe" Nothing (Just eru)
                 aule = Sheep "Aule" Nothing (Just eru)
                 gimli = Sheep "Gimli" Nothing (Just aule) 
                 yavanna = Sheep "Yavanna" Nothing (Just eru)
                 molly = Sheep "Molly" (Just yavanna) (Just abel) 
             in Sheep "Dolly" (Just molly) Nothing


-- "Maybe" in Haskell is equivalent to option types in OCaml
-- Similarly, "Just x" = "Some x", "Nothing" = "None"

-- write a function to return the maternal grandmother of s (if it exists)
-- otherwise return "Nothing"
-- Hint: use a case statement (like match in OCaml)
maternalGrandmother :: Sheep -> Maybe Sheep
maternalGrandmother s =


-- write a function to return the mothers paternal grandfather of s
mothersPaternalGrandmother :: Sheep -> Maybe Sheep
mothersPaternalGrandmother s =

main :: IO ()
-- What should be printed?
main = let dolly = breedSheep
        in do print (mothersPaternalGrandmother dolly)


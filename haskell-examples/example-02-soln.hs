-- Breeding sheep without monads

data Sheep = Sheep {name :: String, mother :: Maybe Sheep, father :: Maybe Sheep}

instance Show Sheep where
    show s = show (name s)

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

maternalGrandmother :: Sheep -> Maybe Sheep
maternalGrandmother s = case (mother s) of
                            Nothing -> Nothing
                            Just m -> mother m

-- see how ugly nested case statements can get!
mothersPaternalGrandmother :: Sheep -> Maybe Sheep
mothersPaternalGrandmother s = case (mother s) of
                                Nothing -> Nothing
                                Just m -> case (father m) of
                                            Nothing -> Nothing
                                            Just gf -> mother gf 

main :: IO ()
main = let dolly = breedSheep
        in do print (mothersPaternalGrandmother dolly)


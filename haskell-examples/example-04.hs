-- Breeding sheep with monads: now with do notation
--
-- Monad definition: 
-- class Monad m where
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b

-- instance Monad Maybe where
--     return = Just
--     Nothing >>= f = Nothing
--     (Just x) >>= f = f x

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

-- use the "do ... x <- f y" notation 
maternalGrandmother :: Sheep -> Maybe Sheep
maternalGrandmother s = 

mothersPaternalGrandmother :: Sheep -> Maybe Sheep
mothersPaternalGrandmother s = 

main :: IO ()
main = let dolly = breedSheep
        in do print (mothersPaternalGrandmother dolly)


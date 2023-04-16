import System.Environment
import Data.List

-- example-07.hs
-- Andrew Wong-Rolle, CS 152
-- Let's show how terse Haskell can be!
-- usage: runhaskell example-07.hs <file.txt> OR compile, then ./example-07.hs <file.txt>
-- parses and prints a 2D array from "file.txt"
-- writing this in C or Python would probably involve multiple "for" loops...
-- try it on "matrix*.txt"

main = do 
    args <- getArgs
    -- The "$" symbol indicates everything following is associated more tightly than the preceding
    -- For example, "f $ g x" is equivalent to "f (g x)"
    (case args of [] -> error "usage: ./example-07 <file.txt>"
                  [fname] -> do 
                        contents <- readFile fname
                        putStrLn $ show $ read2DArray contents
                  _ -> error "usage: ./example-07 <file.txt>")

type Row = [Int]
type Matrix = [Row]

-- ensures correct type for read
readInt :: String -> Int 
readInt = read

-- reads a row (list of strings) into a list of ints
-- reads a space separated string into a list of ints
-- words :: String -> [String]
--       breaks a string into a list of strings separated on spaces 
readRow :: String -> Row 
readRow = (map readInt) . words

-- parses a newline separated list of space separated ints into a 2D array
-- lines :: String -> [String]
--       breaks string into a list of strings separated on newlines 
read2DArray :: String -> Matrix 
read2DArray = (map readRow) . lines


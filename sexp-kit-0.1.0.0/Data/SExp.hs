{-# LANGUAGE QuasiQuotes #-}

module Data.SExp
  ( module Data.SExp.Data
  , module Data.SExp.Parser
  ) where

import Data.SExp.Data
import Data.SExp.Parser

haskellSExp :: SExp
haskellSExp = [sexp| (haskell var) |]

haskellInteger :: Integer
haskellInteger = 42

example :: SExp
example = 
  [sexp|
    (this is an s-expression
     # this is a comment
     #| this is a #| nested |# comment |#
     #(this is a 
       commented expression)
     this is an antiquote
     @:haskellSExp
     and this is too
     @int:haskellInteger
     the supported codes are the empty code for 
     an entire s-expression and
     (lit num int dbl str sym)
     you can also build (cons . lists) 
     .
     (antiquoting works in patterns too))
  |]

main :: IO ()
main = putStrLn $ show example

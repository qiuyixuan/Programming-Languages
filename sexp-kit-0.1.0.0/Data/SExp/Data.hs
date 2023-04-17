{-# LANGUAGE TemplateHaskell #-}

module Data.SExp.Data where

import Data.Maybe
import Util.ToQQ
import qualified Language.Haskell.TH as TH

data Number =
    IntegerN Integer
  | DoubleN Double
  deriving (Eq, Ord)

data Lit =
    NumberL Number
  | StringL String
  deriving (Eq, Ord)

data Anti =
    ValA
  | LitA
  | NumA
  | IntA
  | DblA
  | StrA
  | SymA
  deriving (Eq, Ord, Show, Enum)

data SExp =
    LitS Lit
  | SymS String
  | NullS
  | ConsS SExp SExp
  | AntiS Anti String
  deriving (Eq, Ord)

sexpFromList :: [SExp] -> Maybe SExp -> SExp
sexpFromList [] _ = NullS
sexpFromList ss cons = foldr ConsS (fromMaybe NullS cons) ss

sexpToList :: SExp -> Maybe ([SExp], Maybe SExp)
sexpToList NullS = Just ([], Nothing)
sexpToList (ConsS s1 s2) = Just $ consToList s1 s2
sexpToList _ = Nothing

consToList :: SExp -> SExp -> ([SExp], Maybe SExp)
consToList s1 NullS = ([s1], Nothing)
consToList s1 (ConsS s2 s3) =
  let (ss, cons) = consToList s2 s3
  in (s1:ss, cons)
consToList s1 s2 = ([s1], Just s2)

instance Show Number where
  show (IntegerN i) = show i
  show (DoubleN d) = show d

instance Show Lit where
  show (NumberL n) = show n
  show (StringL s) = show s

instance Show SExp where
  show (LitS l) = show l
  show (SymS s) = s
  show NullS = "()"
  show (ConsS sl sr) =
    let (ss, cons) = consToList sl sr
        consS = case cons of
          Nothing -> ""
          Just s -> " . " ++ show s
    in "(" ++ unwords (map show ss) ++ consS ++ ")"
  show (AntiS anti s) =
    let antiCode a = case a of
          ValA -> ""
          LitA -> "lit"
          NumA -> "num"
          IntA -> "int"
          DblA -> "dbl"
          StrA -> "str"
          SymA -> "sym"
    in "@" ++ antiCode anti ++ ":" ++ s
        
-- QQ

instance ToQQ Number where
  toQQE (IntegerN i) = TH.conE 'IntegerN `TH.appE` toQQE i
  toQQE (DoubleN d) = TH.conE 'DoubleN `TH.appE` toQQE d
  toQQP (IntegerN i) = TH.conP 'IntegerN [toQQP i]
  toQQP (DoubleN d) = TH.conP 'DoubleN [toQQP d]

instance ToQQ Lit where
  toQQE (NumberL n) = TH.conE 'NumberL `TH.appE` toQQE n
  toQQE (StringL s) = TH.conE 'StringL `TH.appE` toQQE s
  toQQP (NumberL n) = TH.conP 'NumberL [toQQP n]
  toQQP (StringL s) = TH.conP 'StringL [toQQP s]

instance ToQQ SExp where
  toQQE (LitS l) = TH.conE 'LitS `TH.appE` toQQE l
  toQQE (SymS s) = TH.conE 'SymS `TH.appE` toQQE s
  toQQE NullS = TH.conE 'NullS
  toQQE (ConsS e1 e2) = TH.conE 'ConsS `TH.appE` toQQE e1 `TH.appE` toQQE e2
  toQQE (AntiS anti nameM) = do
    let nameVarE = mkNameE nameM
    case anti of
      ValA -> nameVarE
      LitA -> TH.conE 'LitS `TH.appE` nameVarE
      NumA -> TH.conE 'LitS `TH.appE` (TH.conE 'NumberL `TH.appE` nameVarE)
      IntA -> TH.conE 'LitS `TH.appE` (TH.conE 'NumberL `TH.appE` (TH.conE 'IntegerN `TH.appE` nameVarE))
      DblA -> TH.conE 'LitS `TH.appE` (TH.conE 'NumberL `TH.appE` (TH.conE 'DoubleN `TH.appE` nameVarE))
      StrA -> TH.conE 'LitS `TH.appE` (TH.conE 'StringL `TH.appE` nameVarE)
      SymA -> TH.conE 'SymS `TH.appE` nameVarE
  toQQP (LitS l) = TH.conP 'LitS [toQQP l]
  toQQP (SymS s) = TH.conP 'SymS [toQQP s]
  toQQP NullS = TH.conP 'NullS []
  toQQP (ConsS e1 e2) = TH.conP 'ConsS [toQQP e1, toQQP e2]
  toQQP (AntiS anti nameM) = do
    let nameVarP = mkNameP nameM
    case anti of
      ValA -> nameVarP
      LitA -> TH.conP 'LitS [nameVarP]
      NumA -> TH.conP 'LitS [TH.conP 'NumberL [nameVarP]]
      IntA -> TH.conP 'LitS [TH.conP 'NumberL [TH.conP 'IntegerN [nameVarP]]]
      DblA -> TH.conP 'LitS [TH.conP 'NumberL [TH.conP 'DoubleN [nameVarP]]]
      StrA -> TH.conP 'LitS [TH.conP 'StringL [nameVarP]]
      SymA -> TH.conP 'SymS [nameVarP]
  

mkNameE :: String -> TH.Q TH.Exp
mkNameE "_" = fail "wildcard can only appear in pattern"
mkNameE name = TH.varE $ TH.mkName name

mkNameP :: String -> TH.Q TH.Pat
mkNameP "_" = TH.wildP
mkNameP name = TH.varP $ TH.mkName name

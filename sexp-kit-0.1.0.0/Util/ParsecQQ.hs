module Util.ParsecQQ where

import Text.Printf
import Util.ToQQ
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Text.Parsec as P

mkQQ :: (ToQQ a) => (String -> Either P.ParseError a) -> TH.QuasiQuoter
mkQQ parser = TH.QuasiQuoter expQ patQ undefined undefined
  where
    expQ input = do
      locMsg <- locationError
      case parser input of
        Left e ->
          fail $ "\n" ++ locMsg ++ "\n" ++ show e
        Right s ->
          toQQE s
    patQ input = do
      locMsg <- locationError
      case parser input of
        Left e ->
          fail $ "\n" ++ locMsg ++ "\n" ++ show e
        Right s ->
          toQQP s
    locationError = do
      (TH.Loc fn _ _ (sl, sc) (el, ec)) <- TH.location
      return $ printf "QQ %s[%s:%s-%s:%s]" 
                      fn (show sl) (show sc) (show el) (show ec)

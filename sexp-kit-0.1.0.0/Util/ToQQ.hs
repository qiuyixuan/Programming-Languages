module Util.ToQQ where

import Control.Monad
import qualified Language.Haskell.TH as TH

class ToQQ a where
  toQQE :: a -> TH.Q TH.Exp
  toQQP :: a -> TH.Q TH.Pat

instance ToQQ Integer where
  toQQE = TH.litE . TH.integerL
  toQQP = TH.litP . TH.integerL

instance ToQQ Double where
  toQQE = TH.litE . TH.doublePrimL . toRational
  toQQP = TH.litP . TH.doublePrimL . toRational

instance ToQQ Char where
  toQQE = TH.litE . TH.charL
  toQQP = TH.litP . TH.charL

instance (ToQQ a) => ToQQ [a] where
  toQQE = liftM TH.ListE . mapM toQQE
  toQQP = liftM TH.ListP . mapM toQQP

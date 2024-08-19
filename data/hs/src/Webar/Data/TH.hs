module Webar.Data.TH
  ( ProductOptions (..),
    defaultProductOptions,
    SumOptions (..),
    defaultSumOptions,
    camelTo2,
    deriveProdData,
    deriveSumData,
  )
where

import Control.Applicative (Applicative (liftA2))
import Language.Haskell.TH
import Webar.Data.Cbor.TH (deriveProdCbor, deriveSumCbor)
import Webar.Data.Internal.TH
import Webar.Data.Json.TH (deriveProdJSON, deriveSumJSON)

deriveProdData :: ProductOptions -> Name -> DecsQ
deriveProdData opt n =
  liftA2
    (++)
    (deriveProdCbor opt n)
    (deriveProdJSON opt n)

deriveSumData :: SumOptions -> Name -> DecsQ
deriveSumData opt n =
  liftA2
    (++)
    (deriveSumCbor opt n)
    (deriveSumJSON opt n)

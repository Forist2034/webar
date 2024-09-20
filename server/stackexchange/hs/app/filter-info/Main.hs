{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Data.ByteString as BS
import Data.Proxy
import Data.Text (Text)
import qualified Data.Vector as V
import System.Environment (getArgs)
import Webar.Data.Json (encodeStrictBs)
import Webar.Data.Json.TH
import Webar.Server.StackExchange.RestApi.Filter
import Webar.Server.StackExchange.RestApi.Model

defWrapper :: V.Vector Text
defWrapper =
  V.fromList
    [ "backoff",
      "has_more",
      "items",
      "quota_max",
      "quota_remaining",
      "type"
    ]

data FilterArgs = FilterArgs
  { ffSafe :: Bool,
    ffBase :: Maybe Text,
    ffWrapper :: V.Vector Text,
    ffItem :: Fields
  }

deriveProdToJSON
  defaultProductOptions {fieldLabelModifier = camelTo2 '_' . drop 2}
  ''FilterArgs

mkFilter :: (ToFilter t) => Proxy t -> FilterArgs
mkFilter p =
  FilterArgs
    { ffSafe = False,
      ffBase = Just "none",
      ffWrapper = defWrapper,
      ffItem = filterFields p
    }

main :: IO ()
main = do
  [dest] <- getArgs
  BS.writeFile
    dest
    ( encodeStrictBs
        TypeMap
          { tmAnswer = mkFilter @Answer Proxy,
            tmBadge = mkFilter @Badge Proxy,
            tmComment = mkFilter @Comment Proxy,
            tmCollective = mkFilter @Collective Proxy,
            tmQuestion = mkFilter @Question Proxy,
            tmRevision = mkFilter @Revision Proxy,
            tmTag = mkFilter @Tag Proxy,
            tmTagSynonym = mkFilter @TagSynonym Proxy,
            tmTagWiki = mkFilter @TagWiki Proxy,
            tmUser = mkFilter @User Proxy,
            tmInfo = mkFilter @Info Proxy
          }
    )

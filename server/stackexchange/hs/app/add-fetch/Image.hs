{-# LANGUAGE OverloadedStrings #-}

module Image
  ( ImageSet (..),
    printError,
    addImageUrl,
    writeImageSpec,
    writeUrlSet,
    addContentImage,
  )
where

import Control.Exception (SomeException)
import qualified Data.ByteString as BS
import Data.Foldable
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.HTML.TagSoup
import Text.URI
import qualified Webar.Data.Cbor as Cbor
import qualified Webar.Data.Json as Json
import Webar.Image.Fetcher.Generic
import Webar.Server.StackExchange.Image.Source

data ImageSet = ImageSet
  { images :: !(S.Set ArchiveImage),
    invalidUrls :: !(M.Map Text SomeException),
    unknownUrls :: !(S.Set Text)
  }

instance Semigroup ImageSet where
  l <> r =
    ImageSet
      { images = S.union (images l) (images r),
        invalidUrls = M.union (invalidUrls l) (invalidUrls r),
        unknownUrls = S.union (unknownUrls l) (unknownUrls r)
      }

instance Monoid ImageSet where
  mempty =
    ImageSet
      { images = S.empty,
        invalidUrls = M.empty,
        unknownUrls = S.empty
      }

printError :: String -> ImageSet -> IO ()
printError ctx is = do
  traverse_
    (\(u, e) -> putStrLn (ctx ++ ": invalid url: " ++ show u ++ " " ++ show e))
    (M.toList (invalidUrls is))
  traverse_ (\u -> putStrLn (ctx ++ ": unknown url " ++ show u)) (unknownUrls is)

writeImageSpec :: Maybe FilePath -> ImageSet -> IO ()
writeImageSpec (Just p) ImageSet {images = is} =
  BS.writeFile
    p
    ( Cbor.encodeStrictBs
        ( V.fromListN
            (S.size is)
            ( fmap
                ( \i@(AImgContent txt) ->
                    ImageSpec
                      { imgId = i,
                        imgPreferredUrl = "https://i.sstatic.net/" <> txt,
                        imgOtherUrls = V.empty
                      }
                )
                (S.toAscList is)
            )
        )
    )
writeImageSpec Nothing _ = pure ()

writeUrlSet :: Maybe FilePath -> S.Set Text -> IO ()
writeUrlSet (Just p) us
  | not (S.null us) = BS.writeFile p (Json.encodeStrictBs us)
  | otherwise = pure ()
writeUrlSet Nothing _ = pure ()

parseId :: URI -> Maybe ArchiveImage
parseId
  URI
    { uriAuthority = Right Authority {authHost = h},
      uriPath = Just (_, p :| [])
    }
    | unRText h == "i.sstatic.net" || unRText h == "i.stack.imgur.com" =
        let (idWithDot, ext) = T.breakOnEnd "." (unRText p)
         in case T.unsnoc idWithDot of
              Just (iid, '.') -> case T.length iid of
                5 -> Just (mkId iid ext)
                6 -> Just (mkId (T.init iid) ext) -- image with resize
                8 -> Just (mkId iid ext)
                9 -> Just (mkId (T.init iid) ext) -- image with resize
                _ -> Nothing
              _ -> Nothing
    where
      mkId t ext = AImgContent (T.concat [t, ".", ext])
parseId _ = Nothing

addImageUrl :: Text -> ImageSet -> ImageSet
addImageUrl u s =
  case mkURI u of
    Right url -> case parseId url of
      Just i -> s {images = S.insert i (images s)}
      Nothing -> s {unknownUrls = S.insert u (unknownUrls s)}
    Left err ->
      s {invalidUrls = M.insert u err (invalidUrls s)}

addContentImage :: Text -> ImageSet -> ImageSet
addContentImage cont imgSet =
  foldl'
    ( \set t -> case t of
        TagOpen "img" attrs -> case L.lookup "src" attrs of
          Just src -> addImageUrl src set
          Nothing -> set
        _ -> set
    )
    imgSet
    (parseTags cont)
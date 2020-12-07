module HGamer3D.Input.Hover

where

import Fresco
import Data.ByteString
import HGamer3D.Data.Vector

import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Monoid
import Control.Applicative

ctHoverCamera :: ComponentType ByteString
ctHoverCamera = ComponentType 0xc9947e814b1f9b89

ctHoverEvent :: ComponentType HoverEvent
ctHoverEvent = ComponentType 0x76492fa6aa357311

type EntityId = ByteString

-- | a hover event is either: hovering over nothing, or hovering over an entity with fields
newtype HoverEvent = HoverEvent { toMaybe :: Maybe HoverEntity }
--  deriving (Eq, Ord, Show)

data HoverEntity = HoverEntity
  { hitEntityId :: EntityId
  , hitPosition :: Vec3
  } deriving (Show)

instance Serialise (HoverEvent) where
  encode (HoverEvent Nothing) = encodeListLen 1 <> encode (0::Int)
  encode (HoverEvent (Just (HoverEntity v1 v2))) = encodeListLen 3 <> encode (1::Int) <> encode v1 <> encode v2
  decode = do
    decodeListLen
    i <- decode :: Decoder s Int
    case i of
      0 -> (pure (HoverEvent Nothing))
      1 -> do
        hoverEntity <- decode
        return $ HoverEvent (Just hoverEntity)

instance Serialise (HoverEntity) where
  encode (HoverEntity hitEntityId hitPosition) = encode hitEntityId <> encode hitPosition
  decode = do
           e <- decode
           pos <- decode
           return $ HoverEntity e pos

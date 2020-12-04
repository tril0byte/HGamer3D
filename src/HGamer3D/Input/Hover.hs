module HGamer3D.Input.Hover

where

import Fresco
import Data.ByteString
import Data.Vect.Float.Base

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
newtype HoverEvent = HoverEvent { toMaybe :: Maybe EntityId }
  deriving (Eq, Ord, Show)

-- CBOR already has a Serialize instance for (Maybe a),
-- but it doesn't include the list length.
-- I've made a newtype wrapper so we can leave the list length in here,
-- because there's a high chance that Fresco needs everything prefixed by length;
-- and if not, it doesn't hurt anyway.
instance Serialise (HoverEvent) where
  encode (HoverEvent Nothing) = encodeListLen 1 <> encode (0::Int)
  encode (HoverEvent (Just v1)) = encodeListLen 2 <> encode (1::Int) <> encode v1
  decode = do
    decodeListLen
    i <- decode :: Decoder s Int
    case i of
      0 -> (pure (HoverEvent Nothing))
      1 -> do
        entityId <- decode
        return $ HoverEvent (Just entityId)

-- | Hover Mode defaults to Hover.
-- Set it to Drag when beginning to drag the item.
-- Set it back to Hover when completing the drag.
ctHoverMode :: ComponentType HoverMode
ctHoverMode = ComponentType 0x7ba50d9aa1400198

data HoverMode = Hover | Drag
  deriving (Eq, Ord, Show)

-- When in Drag mode, the component updates with the relative world coordinates.
-- Vector from object's original (current) position to where it's been dragged so far.
ctDragVector :: ComponentType Vec3
ctDragVector = ComponentType 0xb292cb6a4b805d19

module HGamer3D.Input.Drag

where

import Fresco
import Data.ByteString
import Data.Vect.Float.Base
import HGamer3D.Data.Vector

import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Monoid
import Control.Applicative

-- Serialise instance for Vec3 defined in HGamer3D.Data.Vector
-- exported by module HGamer3D
ctDragHitPosition :: ComponentType Vec3
ctDragHitPosition = ComponentType 0x3fd1f759fcea70b4

-- EntityId
ctDragCamera :: ComponentType ByteString
ctDragCamera = ComponentType 0x87c2f05611cb2d50

data DragEvent = DragEnding | DragActive Vec3

ctDragEvent :: ComponentType DragEvent
ctDragEvent = ComponentType 0xa855430c4f4b1212

instance Serialise DragEvent where
  encode DragEnding = encodeListLen 1 <> encode (0::Int)
  encode (DragActive v1) = encodeListLen 2 <> encode (1::Int) <> encode v1
  decode = do
    decodeListLen
    i <- decode :: Decoder s Int
    case i of
      0 -> pure DragEnding
      1 -> do
        dragPosition <- decode
        return $ DragActive dragPosition

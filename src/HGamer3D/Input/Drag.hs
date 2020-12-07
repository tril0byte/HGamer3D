module HGamer3D.Input.Drag

where

import Fresco
import Data.ByteString
import Data.Vect.Float.Base

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

ctDragEvent :: ComponentType Vec3
ctDragEvent = ComponentType 0xa855430c4f4b1212

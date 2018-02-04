{-# LANGUAGE OverloadedStrings #-}

module Sample_10_3DText where

import HGamer3D
import Control.Concurrent
import Control.Monad
import SampleRunner

creator hg3d = do

    eGeo <- newE hg3d [
--        ctGeometry #: ShapeGeometry Cube,
--        ctMaterial #: matBlue,
--        ctScale #: Vec3 10.0 10.0 10.0,
        ctPosition #: Vec3 0.0 0.0 0.0,
        ctOrientation #: unitU,

        ctText3D #: Text3D "Fonts/BlueHighway.ttf" 24 FCNone True,
        ctLabel #: "Blauer Kubus",
        ctVisible #: True
        ]

    quitV <- makeVar False

    let rotateCube = do
                updateC eGeo ctOrientation (\u -> (rotU vec3Z 0.02) .*. u)
                updateC eGeo ctOrientation (\u -> (rotU vec3X 0.015) .*. u)
                sleepFor (msecT 12)
                q <- readVar quitV
                if not q
                  then rotateCube
                  else return ()

    forkIO rotateCube

    let blinkCube = do
          updateC eGeo ctVisible (\v -> not v)
          sleepFor (secT 2)
          blinkCube

    forkIO blinkCube

    return (eGeo, quitV)

destructor (eGeo, quitV) = do
  writeVar quitV True
  sleepFor (msecT 500) -- monitor that cube stops before deletion
  delE eGeo
  return ()

sampleRunner hg3d = SampleRunner (return ()) (do
                                    state <- creator hg3d
                                    return (SampleRunner (destructor state) (return emptySampleRunner) ))


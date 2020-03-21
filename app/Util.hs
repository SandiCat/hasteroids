module Util where

import qualified Graphics.Gloss.Interface.Pure.Display
                                               as Gloss
import qualified Data.Vect.Float               as Vec
import           Data.Vect.Float                ( (*&)
                                                , (&+)
                                                , (&-)
                                                )
import           Data.Vect.Float                ( Vec2 )

type Polygon = [Vec2]

toPoint :: Vec2 -> Gloss.Point
toPoint (Vec.Vec2 x y) = (x, y)

fullCircle :: Float
fullCircle = 2 * pi

radToDeg :: Floating a => a -> a
radToDeg = (* (180 / pi))

degToRad :: Floating a => a -> a
degToRad = (* (pi / 180))

mapIf :: Bool -> (p -> p) -> p -> p
mapIf True f a = f a
mapIf False _ a = a

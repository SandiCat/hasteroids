{-# LANGUAGE NoImplicitPrelude #-}

module Player where

import           Prelude                 hiding ( Down )
import           Graphics.Gloss.Interface.Pure.Display
import           Graphics.Gloss.Interface.Pure.Game
import qualified Data.Vect.Float               as Vec
import Data.Vect.Float ( (*&), (&*), (&+), (&-), Vec2 )
import qualified Control.Monad.Random.Lazy     as Rand
import           Util
import qualified Physics
import qualified Optics
import           Optics                         ( (%) )
import           Optics.Operators

data Turning = CW | CCW deriving (Eq)

data Player = Player
    { _physics :: Physics.PhysicsBody
    , _direction :: Vec2
    , _isMoving :: Bool
    , _turning :: Maybe Turning
    }
Optics.makeLenses ''Player

initial :: Player
initial = Player { _physics = Physics.PhysicsBody Vec.zero Vec.zero 10
                 , _direction = Vec.Vec2 0 1
                 , _isMoving = False
                 , _turning = Nothing
                 }

eventUpdate :: Event -> Player -> Player
eventUpdate event model = case event of
    EventKey (SpecialKey KeyUp) Down _ _ -> model & isMoving .~ True
    EventKey (SpecialKey KeyUp) Up _ _ -> model & isMoving .~ False

    EventKey (SpecialKey KeyLeft) Down _ _ -> model & turning .~ Just CCW
    EventKey (SpecialKey KeyRight) Down _ _ -> model & turning .~ Just CW
    EventKey (SpecialKey KeyLeft) Up _ _ -> model & turning .~ Nothing
    EventKey (SpecialKey KeyRight) Up _ _ -> model & turning .~ Nothing

    _ -> model

speed = 100
angularSpeed = 1

timeUpdate :: Float -> Player -> Player
timeUpdate dt model =
    model
        & (physics %~ Physics.update
              dt
              (if model ^. isMoving
                  then [(*&) speed $ model ^. direction]
                  else []
              )
          )
        & (case model ^. turning of
              Just dir -> direction
                  %~ Vec.rotate2 (mapIf (dir == CW) negate  $ dt * angularSpeed)
              Nothing -> id
          )

view :: Player -> Picture
view player =
    uncurry translate (toPoint $ player ^. physics % Physics.position)
        $ rotate
              (negate
              $ radToDeg
              $ Vec.angle2
              $ Vec.rotateCW
              $ player
              ^. direction
              )
        $ color white
        $ scale 5 5
        $ translate (-1) (-2) -- center for the rotation
        $ lineLoop [(0, 0), (1, 4), (2, 0), (1, 1)]

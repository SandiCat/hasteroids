module Player where

import qualified Graphics.Gloss.Interface.Pure.Display
                                               as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game
                                               as Gloss
import qualified Data.Vect.Float               as Vec
import           Data.Vect.Float                ( (*&)
                                                , (&*)
                                                , (&+)
                                                , (&-)
                                                )
import           Data.Vect.Float                ( Vec2 )
import qualified Control.Monad.Random.Lazy     as Rand
import           Data.List.HT                   ( rotate )
import           Util
import qualified Physics
import qualified Optics
import           Optics                         ( (%) )
import           Optics.Operators
import           Util

data Player = Player
    { _physics :: Physics.PhysicsBody
    }
Optics.makeLenses ''Player

initial :: Player
initial = Player { _physics = Physics.PhysicsBody Vec.zero (Vec.Vec2 0 1) 10 }

update :: Gloss.Event -> Player -> Player
update event model =
    case event of
        Gloss.EventKey (Gloss.SpecialKey Gloss.KeyLeft) _ _ _ ->
            model
        _ ->
            model

view :: Player -> Gloss.Picture
view player =
    uncurry Gloss.translate (toPoint $ player ^. physics % Physics.position)
        $ Gloss.scale 5 5
        $ Gloss.color Gloss.white
        $ Gloss.lineLoop [(0, 0), (1, 4), (2, 0), (1, 1)]

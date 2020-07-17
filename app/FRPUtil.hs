module FRPUtil where

import Data.Sequence ((<|), Seq (..), ViewL (..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Vect.Float as Vec
import Data.Vect.Float ((&*), (&+), (&-), (*&))
import Data.Vect.Float (Vec2 (..))
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding (Event)
import Reactive.Banana
import Reactive.Banana.Frameworks (MomentIO)
import ReactiveGloss
import Prelude hiding (Down)

test :: GlossNetwork -> IO ()
test = playBanana (InWindow "frp" (800, 600) (0, 0)) white 60

type GlossNetwork = Event Float -> Event InputEvent -> MomentIO (Behavior Picture)

mkMousePos ::
  MonadMoment m =>
  Event InputEvent ->
  m (Behavior Vec2)
mkMousePos eEvent =
  eEvent
    <&> ( \case
            EventMotion x -> Just x
            _ -> Nothing
        )
    & filterJust
    <&> uncurry Vec2
    & stepper (Vec2 0 0)

keyEvent eEvent key =
  eEvent
    <&> ( \case
            EventKey keyGot keyState _ _ ->
              if keyGot == key
                then Just keyState
                else Nothing
            _ ->
              Nothing
        )
    & filterJust

mkMouseClicks ::
  MonadMoment m =>
  KeyState ->
  MouseButton ->
  Event InputEvent ->
  m (Event Vec2)
mkMouseClicks keyState button eEvent = do
  mousePos <- mkMousePos eEvent
  return $
    keyEvent eEvent (MouseButton button)
      & filterE (== keyState)
      & (<@) mousePos

clicks :: GlossNetwork
clicks eTick eEvent = do
  mousePos <- mkMousePos eEvent
  allClicks <-
    keyEvent eEvent (MouseButton LeftButton)
      & filterE (\case Up -> True; Down -> False)
      & (<@) mousePos
      & fmap (:)
      & accumE []
  allClicks
    <&> map (\(Vec2 x y) -> translate x y $ circle 20)
    <&> pictures
    & stepper mempty

subsequent :: MonadMoment m => (a, a) -> Event a -> m (Event (a, a))
subsequent start e =
  e
    <&> (\x (_, prev) -> (prev, x))
    & accumE start

vecEq v1 v2 =
  (< 2) $ Vec.norm $ v1 &- v2

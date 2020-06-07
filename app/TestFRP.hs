{-# LANGUAGE RecursiveDo #-}

module TestFRP where

import Data.Sequence ((<|), Seq (..), ViewL (..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Vect.Float as Vec
import Data.Vect.Float ((&*), (&+), (&-), (*&))
import Data.Vect.Float (Vec2)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding (Event)
import Reactive.Banana
import Reactive.Banana.Frameworks (MomentIO)
import ReactiveGloss
import Prelude hiding (Down)

test :: GlossNetwork -> IO ()
test = playBanana (InWindow "frp" (800, 600) (0, 0)) white 60

type GlossNetwork = Event Float -> Event InputEvent -> MomentIO (Behavior Picture)

sinCircle :: GlossNetwork
sinCircle eTick eEvent = do
  timeElapsed <- accumE 0 $ fmap (+) eTick
  timeElapsedB <- stepper 0 timeElapsed
  pure $
    timeElapsedB <&> \t ->
      Circle $ (100 *) $ sin t

mkMousePos ::
  MonadMoment m =>
  Event InputEvent ->
  m (Behavior (Float, Float))
mkMousePos eEvent =
  eEvent
    <&> ( \case
            EventMotion x -> Just x
            _ -> Nothing
        )
    & filterJust
    & stepper (0, 0)

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
  Event InputEvent ->
  m (Event (Float, Float))
mkMouseClicks eEvent = do
  mousePos <- mkMousePos eEvent
  return $
    keyEvent eEvent (MouseButton LeftButton)
      & filterE (\case Up -> True; Down -> False)
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
    <&> map (\(x, y) -> translate x y $ circle 20)
    <&> pictures
    & stepper mempty

moveCommand :: GlossNetwork
moveCommand eTick eEvent = mdo
  currentDelta <- stepper 0 eTick
  mouseClicks <- (uncurry Vec.Vec2 <$>) <$> mkMouseClicks eEvent
  commands <-
    accumB Seq.empty $
      unions
        [ whenE reachedDestination eTick -- sample on tick i guess
            <&> (\_ -> Seq.drop 1),
          mouseClicks
            <&> flip (|>)
        ]
  let reachedDestination :: Behavior Bool
      reachedDestination =
        ( \pos cmds ->
            case Seq.viewl cmds of
              goal :< _ ->
                (< 0.01) $ Vec.norm $ pos &- goal
              EmptyL ->
                False
        )
          <$> unitPosition
          <*> commands
      unitPosition :: Behavior Vec2
      unitPosition =
        ( \v0 cmds dt ->
            case Seq.viewl cmds of
              v1 :< _ ->
                (*&) (dt * speed) $ Vec.normalize $ v1 &- v0
              Seq.EmptyL ->
                v0
        )
          <$> unitPosition
          <*> commands
          <*> currentDelta
      speed :: Float
      speed = 1
      pic :: Behavior Picture
      pic =
        ( \(Vec.Vec2 x y) cmds ->
            toList cmds
              & map (\(Vec.Vec2 x y) -> translate x y $ circle 2)
              & ((translate x y $ circle 5) :)
              & pictures
        )
          <$> unitPosition
          <*> commands
  return pic
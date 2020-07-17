{-# LANGUAGE RecursiveDo #-}

module TestFRP where

import Data.Sequence ((<|), Seq (..), ViewL (..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Vect.Float as Vec
import Data.Vect.Float ((&*), (&+), (&-), (*&))
import Data.Vect.Float (Vec2 (..))
import FRPUtil
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game hiding (Event)
import Reactive.Banana
import Reactive.Banana.Frameworks (MomentIO)
import ReactiveGloss
import Prelude hiding (Down)

sinCircle :: GlossNetwork
sinCircle eTick eEvent = do
  timeElapsed <- accumE 0 $ fmap (+) eTick
  timeElapsedB <- stepper 0 timeElapsed
  pure $
    timeElapsedB <&> \t ->
      Circle $ (100 *) $ sin t

-- TODO: rewrite with Behavior Time and linear interpolation between the first two points of clicks
moveCommand :: GlossNetwork
moveCommand eTick eEvent = mdo
  let reachedDestination :: Behavior Bool
      reachedDestination =
        ( \pos cmds ->
            case Seq.viewl cmds of
              goal :< _ ->
                vecEq goal pos
              EmptyL ->
                False
        )
          <$> unitPosition
          <*> commandsB
      speed :: Float
      speed = 50
      dup a = (a, a)
      pic :: Behavior Picture
      pic =
        ( \(Vec.Vec2 x y) cmds ->
            toList cmds
              & map (\(Vec.Vec2 x y) -> translate x y $ circle 2)
              & ((translate x y $ circle 5) :)
              & pictures
        )
          <$> unitPosition
          <*> commandsB
      newGoalE =
        commandsSubsequent
          <&> \case
            (Empty, Empty) -> Nothing
            (Empty, x :<| _) -> Just x
            (x :<| _, y :<| _) -> if vecEq x y then Nothing else Just y
            -- if you make two subsequent closeby commands, this will eat one
            -- probably need to do something else in `mapAccum`
            (_, Empty) -> Nothing
          & filterJust
  mouseClicks <- mkMouseClicks Up LeftButton eEvent
  (commandsE, commandsB) <-
    mapAccum Seq.empty $
      (dup .)
        <$> unions
          [ whenE reachedDestination eTick -- sample on tick i guess
              <&> ( \_ acc ->
                      case Seq.viewl acc of
                        EmptyL -> acc
                        _ :< rest -> rest
                  ),
            mouseClicks
              <&> flip (|>)
          ]
  commandsSubsequent <- subsequent (Seq.empty, Seq.empty) commandsE
  unitPositionTransform <-
    newGoalE
      <&> (\goal -> (eTick <&> \dt v -> v &+ Vec.normalize (goal &- v) &* (dt * speed)))
      & switchE
  unitPosition <- accumB Vec.zero unitPositionTransform
  return pic

selectCommand :: GlossNetwork
selectCommand eTick eEvent = do
  mousePos <- mkMousePos eEvent
  mouseDown <- mkMouseClicks Down LeftButton eEvent
  mouseUp <- mkMouseClicks Up LeftButton eEvent
  firstCorner <-
    unions
      [ mouseDown <&> const . Just,
        mouseUp <&> const . const Nothing
      ]
      & accumB Nothing
  return $
    ( \firstCorner (Vec2 mx my) ->
        case firstCorner of
          Nothing -> mempty
          Just (Vec2 x0 y0) ->
            rectanglePath (abs $ x0 - mx) (abs $ y0 - my)
              & lineLoop
              & translate ((x0 + mx) / 2) ((y0 + my) / 2)
    )
      <$> firstCorner
      <*> mousePos

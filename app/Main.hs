module Main (main) where

import qualified Asteroid
import qualified Control.Monad.Random.Lazy as Rand
import qualified Data.Vect.Float as Vec
import Data.Vect.Float ((&*), (&+), (&-), (*&))
import Data.Vect.Float (Vec2)
import FRPUtil (test)
import qualified Graphics.Gloss.Interface.Pure.Display as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import qualified Graphics.Gloss.Interface.Pure.Simulate as Gloss
import qualified Optics as Optics
import Optics ((%))
import Optics.Operators
import Optics.State.Operators ((%=))
import qualified Physics
import qualified Player
import qualified Rapid
import qualified System.Random as Rand
import qualified TestFRP

screenWidth, screenHeight :: Int
screenWidth = 1000
screenHeight = 750

data Model
  = Model
      { _asteroids :: [Asteroid.Asteroid],
        _player :: Player.Player
      }

Optics.makeLenses ''Model

randomAsteroids :: Rand.MonadRandom m => Int -> m [Asteroid.Asteroid]
randomAsteroids numAsteroids =
  replicateM numAsteroids $ do
    x :: Float <- Rand.getRandomR (screenWidth & fromIntegral & (/ 2) & negate, screenWidth & fromIntegral & (/ 2))
    y :: Float <- Rand.getRandomR (screenHeight & fromIntegral & (/ 2) & negate, screenHeight & fromIntegral & (/ 2))
    body <- Physics.randomBody (Vec.Vec2 x y) 10 10
    Asteroid.randomAsteroid body

view :: Model -> Gloss.Picture
view model =
  (Gloss.pictures $ map Asteroid.view $ model ^. asteroids)
    <> (Player.view $ model ^. player)

timeUpdate :: Float -> Model -> Model
timeUpdate dt = execState $ do
  -- note: this can lead to funky behavior if the player's update depends on the asteroids
  -- it would be nicer if all the fields were guaranteed to update independently based on the
  -- previous model and then reconcile
  asteroids % Optics.traversed % Asteroid.physics %= Physics.update dt []
  player %= Player.timeUpdate dt

eventUpdate :: Gloss.Event -> Model -> Model
eventUpdate event =
  player %~ Player.eventUpdate event

game :: IO ()
game = do
  Rand.setStdGen $ Rand.mkStdGen 31890
  initalAsteroids <- randomAsteroids 20
  let initalModel =
        Model
          { _asteroids = initalAsteroids,
            _player = Player.initial
          }
  Gloss.play
    (Gloss.InWindow "asteroids" (screenWidth, screenHeight) (100, 100))
    (Gloss.greyN 0.1)
    60
    initalModel
    view
    eventUpdate
    timeUpdate

main :: IO ()
main =
  test TestFRP.moveCommand

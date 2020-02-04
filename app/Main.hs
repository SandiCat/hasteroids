module Main (main) where


import qualified Control.Monad.Random.Lazy as Rand
import qualified System.Random as Rand
import qualified Graphics.Gloss.Interface.Pure.Simulate as Gloss
import qualified Graphics.Gloss.Interface.Pure.Display as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import qualified Asteroid
import qualified Physics
import qualified Data.Vect.Float as Vec
import Data.Vect.Float ((*&), (&*), (&+), (&-))
import Data.Vect.Float (Vec2)
import Optics.Operators
import qualified Optics as Optics
import Optics ((%))
import qualified Player
import qualified Rapid 

screenWidth, screenHeight :: Int
screenWidth = 1000
screenHeight = 750

data Model = Model
    { _asteroids :: [Asteroid.Asteroid]
    , _player :: Player.Player
    }
Optics.makeLenses ''Model

randomAsteroids :: Rand.MonadRandom m => Int -> m [Asteroid.Asteroid]
randomAsteroids numAsteroids =
    replicateM numAsteroids $ do
        x :: Float <- Rand.getRandomR (screenWidth & fromIntegral & (/2) & negate, screenWidth & fromIntegral & (/2))
        y :: Float <- Rand.getRandomR (screenHeight & fromIntegral & (/2) & negate, screenHeight & fromIntegral & (/2))
        body <- Physics.randomBody (Vec.Vec2 x y) 10 10
        Asteroid.randomAsteroid body

view :: Model -> Gloss.Picture
view model =
    (Gloss.pictures $ map Asteroid.view $ model ^. asteroids)
    <> (Player.view $ model ^. player)

updateTime :: Float -> Model -> Model
updateTime dt =
    asteroids % Optics.traversed % Asteroid.physics %~ Physics.update dt []

updateEvent :: Gloss.Event -> Model -> Model
updateEvent event model = model

game :: IO ()
game = do
    Rand.setStdGen $ Rand.mkStdGen 31890

    initalAsteroids <- randomAsteroids 20
    let
        initalModel = Model
            { _asteroids = initalAsteroids
            , _player = Player.initial
            }

    Gloss.play
        (Gloss.InWindow "asteroids" (screenWidth, screenHeight) (100, 100))
        (Gloss.greyN 0.1)
        60
        initalModel
        view
        updateEvent
        updateTime

    -- Gloss.simulate
    --     (Gloss.InWindow "asteroids" (screenWidth, screenHeight) (100, 100))
    --     (Gloss.greyN 0.1)
    --     60
    --     model
    --     view
    --     update
    
    -- Gloss.display
    --     (Gloss.InWindow "asteroids" (screenWidth, screenHeight) (100, 100))
    --     (Gloss.greyN 0.1)
    --     (view [model])

main :: IO ()
main = game

-- reload =
--     Rapid.rapid 0 $ \r ->
--         Rapid.restart r "game" $
--             game
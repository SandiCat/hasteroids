module Asteroid where

import qualified Graphics.Gloss.Interface.Pure.Display as Gloss
import qualified Data.Vect.Float as Vec
import Data.Vect.Float ((*&), (&*), (&+), (&-))
import Data.Vect.Float (Vec2)
import qualified Control.Monad.Random.Lazy as Rand
import Data.List.HT (rotate)
import Util
import qualified Physics
import qualified Optics
import Optics ((%))
import Optics.Operators
import Util

data Asteroid = Asteroid 
    { _shape :: Polygon 
    , _physics :: Physics.PhysicsBody
    }
Optics.makeLenses ''Asteroid

randomAsteroidShape :: forall m. Rand.MonadRandom m => m Polygon
randomAsteroidShape = 
    let
        diff = fullCircle / 20
        numPointsRange = (4 :: Int, 7)
        extremityProgression = [0.2, 0.1, 0.05, 0.01]

        iterateJagged :: Float -> [Vec2] -> m [Vec2]
        iterateJagged extremity points =
            fmap mconcat $ forM (zip points $ rotate 1 points) $ \(v1, v2) -> do
                t <- Rand.getRandomR (0 :: Float, 1)
                let v = (1 - t) *& v1 &+ t *& v2
                h <- (\x -> x * extremity * Vec.distance v1 v2) <$> Rand.getRandomR (-1 :: Float, 1)
                let n = (h *&) $ Vec.normalize $ Vec.rotateCW $ v2 &- v1
                return [v1, v &+ n]

    in do
        numPoints <- Rand.getRandomR numPointsRange
        initialPoints <-  forM [0 .. numPoints - 1] $ \i -> do
            d <- Rand.getRandomR (- diff, diff)
            return $ Vec.sinCos $ fromIntegral i * fullCircle / fromIntegral numPoints + d
        
        foldlM (flip iterateJagged) initialPoints extremityProgression

randomAsteroid :: Rand.MonadRandom m => Physics.PhysicsBody -> m Asteroid
randomAsteroid body =
    Asteroid <$> randomAsteroidShape <*> pure body
    
view :: Asteroid -> Gloss.Picture
view asteroid =
    uncurry Gloss.translate (toPoint $ asteroid ^. physics % Physics.position)
    $ Gloss.scale 30 30
    $ Gloss.color Gloss.white
    $ Gloss.lineLoop
    $ map toPoint 
    $ Optics.view shape asteroid

highlightPoints :: [Vec2] -> Gloss.Picture
highlightPoints points = 
    Gloss.color Gloss.yellow $ mconcat 
    $ map (\(Vec.Vec2 x y) -> Gloss.translate x y $ Gloss.circle 0.1) points
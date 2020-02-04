module Physics where

import qualified Data.Vect.Float as Vec
import Data.Vect.Float ((*&), (&*), (&+), (&-))
import Data.Vect.Float (Vec2)
import Optics
import Optics.Operators
import Optics.State.Operators
import qualified Control.Monad.Random.Lazy as Rand
import Util

data PhysicsBody = PhysicsBody
    { _position :: Vec2
    , _velocity :: Vec2
    , _mass :: Float
    }
makeLenses ''PhysicsBody

update :: Float -> [Vec2] -> PhysicsBody -> PhysicsBody
update dt forces body =
    flip execState body $ do
        let accelerations = map (\f -> f &* (1 / body ^. mass)) forces
        let dv = foldl' (&+) Vec.zero $ map (&* dt) accelerations
        v <- velocity <%= (&+) dv
        position %= ((&+) $ (&*) v dt)

randomVec :: Rand.MonadRandom m => Float -> m Vec2
randomVec magnitude = (*&) magnitude . Vec.sinCos <$> Rand.getRandomR (0, fullCircle)

randomBody :: Rand.MonadRandom m => Vec2 -> Float -> Float -> m PhysicsBody
randomBody pos magnitude mass = 
    Physics.PhysicsBody
        <$> pure pos
        <*> randomVec magnitude
        <*> pure mass
    
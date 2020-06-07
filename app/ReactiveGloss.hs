{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Code inspired by https://hackage.haskell.org/package/gloss-banana-0.1.0.4/docs/Graphics-Gloss-Interface-FRP-ReactiveBanana.html
module ReactiveGloss
  ( playBanana,
    InputEvent,
  )
where

-- import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import qualified Graphics.Gloss.Interface.IO.Game as G
import Reactive.Banana
import Reactive.Banana.Frameworks

-- | A useful type synonym for Gloss event values, to avoid confusion between
--   Gloss and ReactiveBanana.
type InputEvent = G.Event

-- | Play the game in a window, updating when the value of the provided
--   Behavior t Picture changes.
playBanana ::
  -- | The display method
  Display ->
  -- | The background colour
  Color ->
  -- | The refresh rate, in Hertz
  Int ->
  -- | A Moment t action to generate the Picture Behavior, taking
  --   the refresh and input Events with respect to which to build it.
  --   The refresh event generates a Float indicating the time delta
  --   since the last refresh.
  ( Event Float ->
    Event InputEvent ->
    MomentIO (Behavior Picture)
  ) ->
  IO ()
playBanana display colour frequency mPicture = do
  pictureref <- newIORef blank
  (tickHandler, tick) <- newAddHandler
  (eventHandler, event) <- newAddHandler
  compile (makeNetwork tickHandler eventHandler $ writeIORef pictureref) >>= actuate
  playIO
    display
    colour
    frequency
    ()
    (\_ -> readIORef pictureref)
    (\ev _ -> () <$ event ev)
    (\time _ -> () <$ tick time)
  where
    makeNetwork tickHandler eventHandler change = do
      eTick <- fromAddHandler tickHandler
      eEvent <- fromAddHandler eventHandler
      bRawPicture <- mPicture eTick eEvent
      -- make sure the Behavior doesn't leak memory if mPicture ignores
      -- one or both kind of events
      undefTick <- stepper undefined eTick
      undefEvent <- stepper undefined eEvent
      let bPicture =
            bRawPicture
              <* undefTick
              <* undefEvent
      changes bPicture >>= reactimate' . fmap (fmap change)
-- initial bPicture >>= liftIO . change
-- -- | Play the game in a window, updating when the value of the provided
-- --   Behavior t Picture changes.
-- playBananaOld ::
--   -- | The display method
--   Display ->
--   -- | The background colour
--   Color ->
--   -- | The refresh rate, in Hertz
--   Int ->
--   -- | A Moment t action to generate the Picture Behavior, taking
--   --   the refresh and input Events with respect to which to build it.
--   --   The refresh event generates a Float indicating the time delta
--   --   since the last refresh.
--   ( Event Float ->
--     Event InputEvent ->
--     MomentIO (Behavior Picture)
--   ) ->
--   IO ()
-- playBananaOld display colour frequency mPicture = do
--   pictureref <- newIORef blank
--   (tickHandler, tick) <- newAddHandler
--   (eventHandler, event) <- newAddHandler
--   compile (makeNetwork tickHandler eventHandler $ writeIORef pictureref) >>= actuate
--   playIO
--     display
--     colour
--     frequency
--     ()
--     (\_ -> readIORef pictureref)
--     (\ev _ -> () <$ event ev)
--     (\time _ -> () <$ tick time)
--   where
--     makeNetwork tickHandler eventHandler change = do
--       eTick <- fromAddHandler tickHandler
--       eEvent <- fromAddHandler eventHandler
--       bRawPicture <- mPicture eTick eEvent
--       -- make sure the Behavior doesn't leak memory if mPicture ignores
--       -- one or both kind of events
--       let bPicture =
--             bRawPicture
--               <* stepper undefined eTick
--               <* stepper undefined eEvent
--       changes bPicture >>= reactimate' . fmap (fmap change)
--       initial bPicture >>= liftIO . change

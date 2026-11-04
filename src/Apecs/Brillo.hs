{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Apecs.Brillo
  ( Camera(..)

  , Brillo.Event(..)
  , Brillo.Key(..)
  , Brillo.SpecialKey(..)
  , Brillo.MouseButton(..)
  , Brillo.KeyState(..)
  , Brillo.Modifiers(..)

  , playWithApecs

  , cameraTransform
  , windowToWorld

  , cfoldDraw
  , cfoldDrawM
  ) where

import Brillo qualified
import Brillo.Interface.IO.Game qualified as Brillo
import Linear
import Apecs qualified

data Camera = Camera
  { camOffset :: V2 Float
  , camScale  :: Float
  } deriving (Eq, Show, Read)

instance Semigroup Camera where
  Camera pos1 scale1 <> Camera pos2 scale2 = Camera (pos1 + pos2) (scale1 * scale2)

instance Monoid Camera where
  mempty = Camera 0 1
  mappend = (<>)

instance Apecs.Component Camera where type Storage Camera = Apecs.Global Camera

playWithApecs
  :: (Apecs.Has w IO Camera)
  => Brillo.Display -- ^ Display mode
  -> Brillo.Color -- ^ Background color
  -> Int -- ^ Desired FPS
  -> Apecs.System w Brillo.Picture -- ^ Drawing function
  -> (Brillo.Event -> Apecs.System w ()) -- ^ Event handling function
  -> (Float -> Apecs.System w ()) -- ^ Stepping function, with a time delta argument.
  -> Apecs.System w ()
playWithApecs disp col fps draw handle step = do
    w <- Apecs.ask
    Apecs.liftIO $ Brillo.playIO disp col fps w draw' handle' step'
  where
    handle' event = Apecs.runSystem $ handle event >> Apecs.ask

    step' dT = Apecs.runSystem $ step dT >> Apecs.ask

    draw' = Apecs.runSystem do
      cam <- Apecs.get Apecs.global
      cameraTransform cam <$> draw

-- | Apply a camera transformation to a picture.
cameraTransform :: Camera -> Brillo.Picture -> Brillo.Picture
cameraTransform (Camera (V2 camX camY) camScale) = Brillo.Scale camScale camScale . Brillo.Translate (-camX) (-camY)

-- | Translates a position from the camera's offset and scale.
windowToWorld :: Camera -> (Float, Float) -> V2 Float
windowToWorld (Camera camPos camScale) (x, y) = V2 x y ^/ camScale + camPos

-- | Renders a picture given a component rendering function.
cfoldDraw :: (Apecs.Get w IO c, Apecs.Members w IO c)
         => (c -> Brillo.Picture) -- ^ Component render function.
         -> Apecs.System w Brillo.Picture
cfoldDraw f = Apecs.cfold (\p -> mappend p . f) mempty

-- | Monadically renders a picture given a component rendering function.
cfoldDrawM :: (Apecs.Get w IO c, Apecs.Members w IO c)
          => (c -> Apecs.System w Brillo.Picture) -- ^ Component render function.
          -> Apecs.System w Brillo.Picture
cfoldDrawM f = Apecs.cfoldM (\p -> fmap (mappend p) . f) mempty

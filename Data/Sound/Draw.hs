
module Data.Sound.Draw (
   -- * Types
   RGBA (..)
 , RenderConfig (..)
 , defaultConfig
   -- * Rendering
 , renderSoundWith
 , renderFileSoundWith
 , renderSound
 , renderFileSound
  ) where

import Data.Sound.Internal
import Graphics.Rendering.Cairo

data RGBA = RGBA { redc   :: !Double
                 , greenc :: !Double
                 , bluec  :: !Double
                 , alphac :: !Double }

data RenderConfig = RC
 { rcwidth     :: Int
 , rcheight    :: Int
 , rcsteps     :: Int
 , signalColor :: RGBA
 , bgColor     :: RGBA
 , axisColor   :: RGBA
 , leftFactor  :: Double
 }

defaultConfig :: RenderConfig
defaultConfig = RC
 { rcwidth     = 800
 , rcheight    = 300
 , rcsteps     =   2
 , signalColor = RGBA 255   0   0 1
 , bgColor     = RGBA   0   0   0 1
 , axisColor   = RGBA 100 100 100 1
 , leftFactor  = recip (2^(4 :: Int))
 }

setColor :: RGBA -> Render ()
setColor (RGBA r g b a) = setSourceRGBA r g b a

move :: (Double,Double) -> Double -> Double -> Render ()
move (a,b) x y = moveTo (x+a) (y+b)

line :: (Double,Double) -> Double -> Double -> Render ()
line (a,b) x y = lineTo (x+a) (y+b)

renderSoundChannel :: RenderConfig -> Sound -> Int -> Render ()
renderSoundChannel rc s ch = do
  ---- Constants
  let w  = fromIntegral (rcwidth  rc)
      h  = fromIntegral (rcheight rc) / fromIntegral (channels s)
      h2 = h/2
      p  = (0, h*(fromIntegral ch - 1))
      lw = leftFactor rc * w
  ---- Axis
  setColor $ axisColor rc
  -- Vertical axis
  move p lw 0
  line p lw h
  stroke
  -- Horizontal axis
  move p 0 h2
  line p w h2
  stroke
  ---- Signal
  setColor $ signalColor rc
  let sw :: Word32
      sw = floor $ w * (1 - leftFactor rc)
  move p lw h2
  mapM_ (\i -> let x :: Word32
                   x = div (i * nSamples s) sw
                   y :: Double
                   y = atSample x s !! (ch-1)
               in  line p (lw + fromIntegral i) (h2*(1-y))
                ) [1 , 1 + fromIntegral (rcsteps rc) .. sw]
  stroke

renderSoundWith :: RenderConfig -> Sound -> Render ()
renderSoundWith rc s = do
   setColor $ bgColor rc
   fill
   mapM_ (renderSoundChannel rc s) [1..n]
 where
  n = channels s

renderSound :: Sound -> Render ()
renderSound = renderSoundWith defaultConfig

renderFileSoundWith :: RenderConfig -> FilePath -> Sound -> IO ()
renderFileSoundWith rc fp s =
  withImageSurface FormatARGB32 (rcwidth rc) (rcheight rc) $
    \sf -> renderWith sf (renderSoundWith rc s) >> surfaceWriteToPNG sf fp

renderFileSound :: FilePath -> Sound -> IO ()
renderFileSound = renderFileSoundWith defaultConfig
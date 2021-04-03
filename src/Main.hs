-- Copyright (C) 2021  Vojtěch Balák
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

module Main where

import Data.Complex
import Data.List as L
import Data.Word
import System.Exit

import Data.ByteString as B
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

type Pixel = (Int, Int)
data World = World { dimension :: (Int, Int)
                   , coloring :: World -> Pixel -> RGB Float
                   , zoom :: Float
                   , center :: (Float, Float) }

stripes :: Pixel -> RGB Float
stripes (x, y) = hsl (fromIntegral ((x + y) `mod` 360)) 1 0.5

pixelToComplex :: World -> Pixel -> Complex Float
pixelToComplex world (x, y) =
    let realRange = 3 / zoom world
        (w, h) = dimension world
        (centerR, centerI) = center world
        toFloat x = fromIntegral x :: Float
        [wf, hf, xf, yf] = toFloat <$> [w, h, x, y]
        r = ((xf - (wf / 2)) / wf) * realRange + centerR
        i = ((yf - (hf / 2)) / hf) * (realRange / wf * hf) + centerI
    in r :+ i

mandelbrot :: World -> Pixel -> RGB Float
mandelbrot world point =
    let c = pixelToComplex world point
        mandelbrot' iter z
          | iter >= 80 = hsl 0 0 0
          | magnitude z >= 2 = hsl (iter / 80 * 360) 1 0.5
          | otherwise = mandelbrot' (iter + 1) (z * z + c)
     in mandelbrot' 0 (0 :+ 0)

rgbToWord :: RGB Float -> [Word8]
rgbToWord rgb =
    let rgbWord = truncate . (*255) <$> rgb
     in [channelRed rgbWord, channelGreen rgbWord, channelBlue rgbWord, 255]

genBitmap :: World -> ByteString
genBitmap world =
    let (w, h) = dimension world
     in B.concat (
            L.unfoldr (
                \i -> if i >= w * h
                      then Nothing
                      else Just (pack $ rgbToWord $ coloring world world (i `mod` w, i `div` w), i + 1)
            ) 0
        )

draw :: World -> IO Picture
draw world = let (w, h) = dimension world
              in return $ bitmapOfByteString w h (BitmapFormat BottomToTop PxRGBA) (genBitmap world) False

handleInput :: Event -> World -> IO World
handleInput event world
    | EventResize (w, h) <- event = return world { dimension = (w, h) }
    | EventKey (Char 'q') Down _ _ <- event = exitSuccess
    | EventKey (Char '+') Down _ _ <- event = return world { zoom = zoom world * 1.5 }
    | EventKey (Char '-') Down _ _ <- event = return world { zoom = zoom world / 1.5 }
    | EventKey (SpecialKey KeyUp) Down _ _ <- event = return $ pan world (0, 0.1)
    | EventKey (SpecialKey KeyDown) Down _ _ <- event = return $ pan world (0, -0.1)
    | EventKey (SpecialKey KeyRight) Down _ _ <- event = return $ pan world (0.1, 0)
    | EventKey (SpecialKey KeyLeft) Down _ _ <- event = return $ pan world (-0.1, 0)
    | otherwise = return world
    where pan world (dx, dy) = world { center = (fst (center world) + (dx / zoom world), snd (center world) + (dy / zoom world)) }

main :: IO ()
main =
    let dimension = (900, 600)
        -- coloring = \_ pixel -> stripes pixel
        coloring = mandelbrot
        world = World { dimension = dimension
                      , coloring = coloring
                      , zoom = 1
                      , center = (-0.5, 0) }
     in playIO (InWindow "hfractal" dimension (0, 0)) white 100 world draw handleInput (\_ w -> return w)

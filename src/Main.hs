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
import Data.List
import Data.Word

import Data.ByteString
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSL
import Graphics.Gloss hiding (Point)

type Point = (Int, Int)
type Dimension = (Int, Int)
type Coloring = Point -> RGB Float

stripes :: Coloring
stripes (x, y) = hsl (fromIntegral ((x + y) `mod` 360)) 1 0.5

pixelToComplex :: Dimension -> Point -> Complex Float
pixelToComplex (w, h) (x, y) =
    let realRange = 3
        centerR = -0.5
        centerI = 0
        toFloat x = fromIntegral x :: Float
        [wf, hf, xf, yf] = toFloat <$> [w, h, x, y]
        r = ((xf - (wf / 2)) / wf) * realRange + centerR
        i = ((yf - (hf / 2)) / hf) * (realRange / wf * hf) + centerI
    in r :+ i

mandelbrot :: Dimension -> Coloring
mandelbrot dim point =
    let c = pixelToComplex dim point
        mandelbrot' iter z
          | iter >= 80 = hsl 0 0 0
          | realPart (abs z) >= 2 = hsl (iter / 80 * 360) 1 0.5
          | otherwise = mandelbrot' (iter + 1) (z * z + c)
     in mandelbrot' 0 (0 :+ 0)

rgbToWord :: RGB Float -> [Word8]
rgbToWord rgb =
    let rgbWord = fmap (truncate . (*255)) rgb
     in [channelRed rgbWord, channelGreen rgbWord, channelBlue rgbWord, 255]

genBitmap :: Dimension -> Coloring -> ByteString
genBitmap (w, h) coloring =
    Data.ByteString.concat (
        Data.List.unfoldr (
            \i -> if i >= w * h
                then Nothing
                else Just (pack $ rgbToWord $ coloring (i `mod` w, i `div` w), i + 1)
        ) 0
    )

picture :: Dimension -> Coloring -> Picture
picture (w, h) coloring = bitmapOfByteString w h (BitmapFormat BottomToTop PxRGBA) (genBitmap (w, h) coloring) False

main :: IO ()
main = do
    let dimension = (1200, 800)
        -- coloring = stripes
        coloring = mandelbrot dimension
     in display (InWindow "hfractal" dimension (0, 0)) white $ picture dimension coloring

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

data Point a = Point { x :: a, y :: a }
data Dimension = Dimension { width :: Int, height :: Int }

stripes :: Point Int -> RGB Float
stripes point = hsl (fromIntegral ((x point + y point) `mod` 360)) 1 0.5

pixelToComplex :: Dimension -> Point Int -> Complex Float
pixelToComplex dim point =
    let realRange = 3
        centerR = -0.5
        centerI = 0
        wf = fromIntegral (width dim) :: Float
        hf = fromIntegral (height dim) :: Float
        xf = fromIntegral (x point) :: Float
        yf = fromIntegral (y point) :: Float
        r = ((xf - (wf / 2)) / wf) * realRange + centerR
        i = ((yf - (hf / 2)) / hf) * (realRange / wf * hf) + centerI
    in r :+ i

mandelbrot :: Dimension -> Point Int -> RGB Float
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

genBitmap :: Dimension -> (Point Int -> RGB Float) -> ByteString
genBitmap dim coloring =
    Data.ByteString.concat (
        Data.List.unfoldr (
            \i -> if i >= width dim * height dim
                then Nothing
                else Just (pack $ rgbToWord $ coloring $ Point (i `mod` width dim) (i `div` width dim), i + 1)
        ) 0
    )

picture :: Dimension -> (Point Int -> RGB Float) -> Picture
picture dim coloring = bitmapOfByteString (width dim) (height dim) (BitmapFormat BottomToTop PxRGBA) (genBitmap dim coloring) False

main :: IO ()
main = do
    let dimension = Dimension 1200 800
        -- coloring = stripes
        coloring = mandelbrot dimension
     in display (InWindow "hfractal" (width dimension, height dimension) (0, 0)) white $ picture dimension coloring

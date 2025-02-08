module Colors where

import Data.Char (digitToInt)
import Data.Word (Word8)
import Game (Tile (tileExponent))
import SDL.Vect
import Text.Printf (printf)

type RGB = V4 Word8

rgb :: String -> RGB
rgb code =
  case code of
    [_, r1, r2, g1, g2, b1, b2] ->
      V4 (convert r1 r2) (convert g1 g2) (convert b1 b2) 255
    _ -> error $ printf "invalid hexcode '%s'" code
  where
    convert :: (Integral a) => Char -> Char -> a
    convert a b = fromIntegral $ 16 * digitToInt a + digitToInt b

tileColor :: Maybe Tile -> RGB
tileColor Nothing = tileEmpty
tileColor (Just tile) =
  case tileExponent tile of
    1 -> tile2
    2 -> tile4
    3 -> tile8
    4 -> tile16
    5 -> tile32
    6 -> tile64
    _ -> tileBig

mainBG :: RGB
mainBG = rgb "#E3DFD4"

gameBG :: RGB
gameBG = rgb "#A09081"

tileEmpty :: RGB
tileEmpty = rgb "#B1A396"

tile2 :: RGB
tile2 = rgb "#D4C8BC"

tile4 :: RGB
tile4 = rgb "#D4C8BA"

tile8 :: RGB
tile8 = rgb "#453A62"

tile16 :: RGB
tile16 = rgb "#5D5086"

tile32 :: RGB
tile32 = rgb "#999999"

tile64 :: RGB
tile64 = rgb "#666666"

tileBig :: RGB
tileBig = rgb "#8F4E8B"

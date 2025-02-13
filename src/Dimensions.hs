module Dimensions where

import Foreign.C (CInt (CInt))

windowWidth :: CInt
windowWidth = 640

windowHeight :: CInt
windowHeight = 800

gameAreaPadding :: CInt
gameAreaPadding = 12

gameGridX :: CInt
gameGridX = windowWidth `div` 18

gameGridY :: CInt
gameGridY = windowHeight `div` 5

gameGridW :: CInt
gameGridW = windowWidth - 2 * gameGridX + gameAreaPadding

gameGridH :: CInt
gameGridH = windowHeight - gameGridX - gameGridY + gameAreaPadding

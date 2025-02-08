module Window
  ( gameWindowInit,
    gameWindowLoop,
  )
where

import Control.Monad (forM_, when)
import Data.Char (digitToInt)
import Data.Text (pack)
import Data.Word (Word8)
import Debug.Trace (traceShow)
import Game (Game, GameResult, Move (East, North, South, West), isGameOver, moveGame)
import SDL
  ( Event (eventPayload),
    EventPayload (KeyboardEvent, WindowClosedEvent),
    InputMotion (Pressed),
    KeyboardEventData (keyboardEventKeyMotion, keyboardEventKeysym),
    Keycode,
    Keysym (keysymKeycode),
    WindowClosedEventData (windowClosedEventWindow),
    ($=),
  )
import SDL.Event (pollEvents)
import SDL.Input.Keyboard.Codes
import SDL.Vect
import SDL.Video
import System.Exit (exitSuccess)
import Text.Printf (printf)

rgb :: String -> V4 Word8
rgb code =
  case code of
    [_, r1, r2, g1, g2, b1, b2] ->
      V4 (convert r1 r2) (convert g1 g2) (convert b1 b2) 255
    _ -> error $ printf "invalid hexcode '%s'" code
  where
    convert :: (Integral a) => Char -> Char -> a
    convert a b = fromIntegral $ 16 * digitToInt a + digitToInt b

gameWindowX :: (Integral a) => a
gameWindowX = 640

gameWindowY :: (Integral a) => a
gameWindowY = 808

gameWindowInit :: IO (Renderer, Window)
gameWindowInit = do
  let title = pack "2048 Solver"
      config =
        defaultWindow {windowInitialSize = V2 gameWindowX gameWindowY}

  window <- createWindow title config
  renderer <- createRenderer window (-1) defaultRenderer

  return (renderer, window)

renderInit :: Renderer -> IO Renderer
renderInit renderer = do
  let colorBG = "#E3DFD4"
      colorEmpty = "#A09081"

  let playSpaceX = gameWindowX `div` 32
      playSpaceY = gameWindowY `div` 6
      playSpaceW = gameWindowX - 2 * playSpaceX
      playSpaceH = gameWindowY - playSpaceY - playSpaceX
      playSpace =
        Rectangle
          (P $ V2 playSpaceX playSpaceY)
          (V2 playSpaceW playSpaceH)

  let spacing = 6
      paddingX = playSpaceW `div` 4 + spacing
      paddingY = playSpaceH `div` 4 + spacing
      emptySpaces =
        [ let rX = playSpaceX + paddingX * (i - 1)
              rY = playSpaceY + paddingY * (j - 1)
              rW = playSpaceW `div` 4 - (2 * spacing)
              rH = playSpaceH `div` 4 - (2 * spacing)
           in traceShow (rX, rY, rW, rH) $ Rectangle (P $ V2 rX rY) (V2 rW rH)
          | i <- [1 .. 4],
            j <- [1 .. 4]
        ]

  rendererDrawColor renderer $= rgb colorBG
  clear renderer

  rendererDrawColor renderer $= rgb colorEmpty
  fillRect renderer (Just playSpace)

  rendererDrawColor renderer $= rgb "#B1A396"
  forM_ emptySpaces (fillRect renderer . Just)

  return renderer

renderGame :: Game -> Renderer -> Renderer
renderGame = undefined

gameWindowLoop :: Game -> Renderer -> Window -> IO ()
gameWindowLoop game renderer window = do
  events <- pollEvents
  startRenderer <- renderInit renderer

  let newGame = foldr handleGameEvent game events
      newRenderer = startRenderer

  present newRenderer

  when (isGameOver game) $ do
    gameQuit window -- TODO: do something better
  gameWindowLoop newGame newRenderer window

pressedKey :: Keycode -> KeyboardEventData -> Bool
pressedKey keycode kbe =
  keyboardEventKeyMotion kbe == Pressed
    && keysymKeycode (keyboardEventKeysym kbe) == keycode

handleGameEvent :: Event -> Game -> Game
handleGameEvent event game =
  case eventPayload event of
    KeyboardEvent kbe
      | pressedKey KeycodeUp kbe -> moveGame North game
      | pressedKey KeycodeDown kbe -> moveGame South game
      | pressedKey KeycodeRight kbe -> moveGame East game
      | pressedKey KeycodeLeft kbe -> moveGame West game
      | otherwise -> game
    _NoMatchingEvent -> game

gameQuit :: Window -> IO ()
gameQuit window = destroyWindow window >> exitSuccess

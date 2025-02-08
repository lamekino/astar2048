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
import Foreign.C (CInt (CInt))
import Game
  ( Game,
    GameResult,
    Move (East, North, South, West),
    isGameOver,
    moveGame,
  )
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
gameWindowY = 800

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
      colorGrid = "#A09081"
      colorEmpty = "#B1A396"

  let spacing = 12

  let gridX = gameWindowX `div` 18
      gridY = gameWindowY `div` 4
      gridW = gameWindowX - 2 * gridX + spacing
      gridH = gameWindowY - gridY - gridX + spacing
      grid =
        Rectangle
          (P $ V2 (gridX - spacing) (gridY - spacing))
          (V2 (gridW + spacing) (gridH + spacing))

  let tileX = gridW `div` 4
      tileY = gridH `div` 4
      emptyTiles =
        [ let rX = tileX * (i - 1) + gridX
              rY = tileY * (j - 1) + gridY
              rW = tileX - spacing
              rH = tileY - spacing
           in Rectangle (P $ V2 rX rY) (V2 rW rH)
          | i <- [1 .. 4],
            j <- [1 .. 4]
        ]

  rendererDrawColor renderer $= rgb colorBG
  clear renderer

  rendererDrawColor renderer $= rgb colorGrid
  fillRect renderer (Just grid)

  rendererDrawColor renderer $= rgb colorEmpty
  forM_ emptyTiles (fillRect renderer . Just)

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

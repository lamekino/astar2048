module Window
  ( windowInit,
    windowLoop,
  )
where

import Control.Monad (unless)
import Data.Text (pack)
import Dimensions (windowHeight, windowWidth)
import Game
  ( Game,
    Move (East, North, South, West),
    isGameOver,
    isGameSolved,
    moveGame,
  )
import Render
import SDL
  ( Event (eventPayload),
    EventPayload (KeyboardEvent, WindowClosedEvent),
    InputMotion (Pressed),
    KeyboardEventData (keyboardEventKeyMotion, keyboardEventKeysym),
    Keysym (keysymKeycode),
    WindowClosedEventData (windowClosedEventWindow),
    waitEvent,
    ($=),
  )
import qualified SDL.Font as TTF
import SDL.Input.Keyboard.Codes
import SDL.Vect (V2 (V2))
import SDL.Video
  ( Renderer,
    Window,
    createRenderer,
    createWindow,
    defaultRenderer,
    defaultWindow,
    destroyWindow,
    present,
    windowInitialSize,
  )
import System.Exit (exitSuccess)

windowInit :: IO (Renderer, Window)
windowInit = do
  let title = pack "2048 Solver"
      config =
        defaultWindow {windowInitialSize = V2 windowWidth windowHeight}

  window <- createWindow title config
  renderer <- createRenderer window (-1) defaultRenderer

  TTF.initialize

  return (renderer, window)

windowLoop :: Game -> Renderer -> Window -> IO ()
windowLoop game renderer window = do
  font <- TTF.load "/usr/share/fonts/TTF/JetBrainsMono-Regular.ttf" 12 -- FIXME:
  tileBank <- preloadTiles font renderer 14
  digitBank <- preloadDigits font renderer

  let loop game' renderer' = do
        maybeGame <- handleGameEvent game' <$> waitEvent

        case maybeGame of
          Nothing -> return ()
          Just newGame -> do
            renderGame digitBank tileBank newGame renderer' font

            present renderer'

            unless (isGameOver newGame || isGameSolved newGame) $ do
              loop newGame renderer'

  loop game renderer

  destroyWindow window
  TTF.free font
  TTF.quit
  exitSuccess

handleGameEvent :: Game -> Event -> Maybe Game
handleGameEvent game event =
  case eventPayload event of
    KeyboardEvent ev
      | pressedKey KeycodeUp ev -> justMove North game
      | pressedKey KeycodeDown ev -> justMove South game
      | pressedKey KeycodeLeft ev -> justMove East game
      | pressedKey KeycodeRight ev -> justMove West game
      | otherwise -> Just game
    WindowClosedEvent _ -> Nothing
    _NoMatchingEvent -> Just game
  where
    justMove :: Move -> Game -> Maybe Game
    justMove move = Just . moveGame move

    pressedKey :: Keycode -> KeyboardEventData -> Bool
    pressedKey keycode ev =
      keyboardEventKeyMotion ev == Pressed
        && keysymKeycode (keyboardEventKeysym ev) == keycode

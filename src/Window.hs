module Window
  ( gameWindowInit,
    gameWindowLoop,
  )
where

import qualified Colors
import Control.Monad (forM_, when)
import qualified Data.Array as Array
import Data.Text (pack)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vec
import Debug.Trace (traceShow, traceShowId)
import Foreign.C (CInt (CInt))
import Game
  ( Game (gameBoard),
    GameResult,
    Move (East, North, South, West),
    Tile (tileExponent),
    isGameOver,
    isGameSolved,
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
import SDL.Vect hiding (Vector)
import SDL.Video
import System.Exit (exitSuccess)

gameWindowX :: CInt
gameWindowX = 640

gameWindowY :: CInt
gameWindowY = 800

gameGridPadding :: CInt
gameGridPadding = 12

gameGridX :: CInt
gameGridX = gameWindowX `div` 18

gameGridY :: CInt
gameGridY = gameWindowY `div` 4

gameGridW :: CInt
gameGridW = gameWindowX - 2 * gameGridX + gameGridPadding

gameGridH :: CInt
gameGridH = gameWindowY - gameGridX - gameGridY + gameGridPadding

gameGrid :: Rectangle CInt
gameGrid =
  Rectangle
    (P $ V2 (gameGridX - gameGridPadding) (gameGridY - gameGridPadding))
    (V2 (gameGridW + gameGridPadding) (gameGridH + gameGridPadding))

gameTiles :: Vector (Rectangle CInt)
gameTiles =
  let tileX = gameGridW `div` 4
      tileY = gameGridH `div` 4
   in Vec.fromList
        [ let rX = tileX * (i - 1) + gameGridX
              rY = tileY * (j - 1) + gameGridY
              rW = tileX - gameGridPadding
              rH = tileY - gameGridPadding
           in Rectangle (P $ V2 rX rY) (V2 rW rH)
          | i <- [1 .. 4],
            j <- [1 .. 4]
        ]

gameWindowInit :: IO (Renderer, Window)
gameWindowInit = do
  let title = pack "2048 Solver"
      config =
        defaultWindow {windowInitialSize = V2 gameWindowX gameWindowY}

  window <- createWindow title config
  renderer <- createRenderer window (-1) defaultRenderer

  return (renderer, window)

renderGame :: Game -> Renderer -> IO Renderer
renderGame game renderer = do
  let indexArray = (Array.!)
      indexVec = (Vec.!)

  let board = gameBoard game

  rendererDrawColor renderer $= Colors.mainBG
  clear renderer

  rendererDrawColor renderer $= Colors.gameBG
  fillRect renderer (Just gameGrid)

  rendererDrawColor renderer $= Colors.tileEmpty
  fillRects renderer gameTiles

  forM_
    (Array.indices board)
    ( \ai@(n, m) ->
        let vi = 4 * (m - 1) + (n - 1)
            curTile = board `indexArray` ai
            curRect = gameTiles `indexVec` vi
         in rendererDrawColor renderer
              $= Colors.tileColor curTile
              >> fillRect renderer (Just curRect)
    )

  return renderer

gameWindowLoop :: Game -> Renderer -> Window -> IO ()
gameWindowLoop game renderer window = do
  events <- pollEvents

  let maybeGame =
        foldr
          (\ev g -> handleGameEvent ev =<< g)
          (Just game)
          events

  case maybeGame of
    Just newGame -> do
      newRenderer <- renderGame newGame renderer

      present newRenderer
      gameWindowLoop newGame newRenderer window
    Nothing -> do
      destroyWindow window
      exitSuccess

handleGameEvent :: Event -> Game -> Maybe Game
handleGameEvent event game
  | isGameOver game = Nothing -- suspect
  | isGameSolved game = Nothing
  | otherwise =
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
    justMove move = Just . traceShowId . moveGame move

    pressedKey :: Keycode -> KeyboardEventData -> Bool
    pressedKey keycode ev =
      keyboardEventKeyMotion ev == Pressed
        && keysymKeycode (keyboardEventKeysym ev) == keycode

module Window
  ( gameWindowInit,
    gameWindowLoop,
  )
where

import qualified Colors
import Control.Monad (forM_, unless, when)
import Data.Array (Array)
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
    createTile,
    isGameOver,
    isGameSolved,
    moveGame,
    tileValue,
  )
import SDL
  ( Event (eventPayload),
    EventPayload (KeyboardEvent, WindowClosedEvent),
    InputMotion (Pressed),
    KeyboardEventData (keyboardEventKeyMotion, keyboardEventKeysym),
    Keysym (keysymKeycode),
    WindowClosedEventData (windowClosedEventWindow),
    ($=),
  )
import SDL.Event (pollEvents)
import qualified SDL.Font as TTF
import SDL.Input.Keyboard.Codes
import SDL.Vect hiding (Vector)
import SDL.Video
import System.Exit (exitSuccess)

type Bank a = Array Int a

indexArray :: (Array.Ix i) => Array i e -> i -> e
indexArray = (Array.!)

indexVec :: (Vec.Storable a) => Vector a -> Int -> a
indexVec = (Vec.!)

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

gamePlayArea :: Rectangle CInt
gamePlayArea =
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

  TTF.initialize

  return (renderer, window)

tileTexture :: TTF.Font -> Renderer -> Tile -> IO Texture
tileTexture font renderer tile =
  -- TODO: resolve tile text color from function
  createTextureFromSurface renderer
    =<< TTF.solid
      font
      (Colors.rgb "#000000")
      (pack $ show (tileValue tile))

preloadTiles :: TTF.Font -> Renderer -> Int -> IO (Bank Texture)
preloadTiles font renderer count =
  Array.listArray (1, count)
    <$> sequence
      [ tileTexture font renderer $ createTile tileNo
        | tileNo <- [1 .. count]
      ]

renderTile ::
  Bank Texture ->
  Maybe Tile ->
  Rectangle CInt ->
  Renderer ->
  TTF.Font ->
  IO Renderer
renderTile tileBank maybeTile rectangle renderer font = do
  rendererDrawColor renderer $= Colors.tileColor maybeTile

  fillRect renderer (Just rectangle)

  case maybeTile of
    Nothing -> return ()
    Just tile -> do
      let idx = tileExponent tile

      useTexture <-
        if Array.inRange (Array.bounds tileBank) idx
          then pure $ tileBank `indexArray` idx -- ahead of time render
          else tileTexture font renderer tile -- jit render (large values)

      -- render the texture to the current rectangle
      copy renderer useTexture Nothing (Just rectangle)

  return renderer

renderGame :: Bank Texture -> Game -> Renderer -> TTF.Font -> IO Renderer
renderGame tileBank game renderer font = do
  let board = gameBoard game

  rendererDrawColor renderer $= Colors.mainBG
  clear renderer

  rendererDrawColor renderer $= Colors.gameBG
  fillRect renderer (Just gamePlayArea)

  forM_
    (Array.indices board)
    ( \ai@(n, m) ->
        let vi = 4 * (m - 1) + (n - 1)
            curTile = board `indexArray` ai
            curRect = gameTiles `indexVec` vi
         in renderTile tileBank curTile curRect renderer font
    )

  return renderer

gameWindowLoop :: Game -> Renderer -> Window -> IO ()
gameWindowLoop game renderer window = do
  -- TODO: change this!
  font <- TTF.load "/usr/share/fonts/TTF/JetBrainsMono-Regular.ttf" 12
  preloaded <- preloadTiles font renderer 14

  let loop curGame curRenderer = do
        maybeGame <- handleEvents curGame

        case maybeGame of
          Nothing -> return ()
          Just newGame -> do
            newRenderer <- renderGame preloaded newGame curRenderer font

            present newRenderer

            unless (isGameOver curGame || isGameSolved curGame) $ do
              loop newGame newRenderer

  loop game renderer

  destroyWindow window
  TTF.free font
  TTF.quit
  exitSuccess

handleEvents :: Game -> IO (Maybe Game)
handleEvents game =
  foldr
    (\ev g -> handleGameEvent ev =<< g)
    (Just game)
    <$> pollEvents

handleGameEvent :: Event -> Game -> Maybe Game
handleGameEvent event game =
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

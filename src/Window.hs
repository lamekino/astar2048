module Window
  ( gameWindowInit,
    gameWindowLoop,
  )
where

import qualified Colors
import Control.Monad (forM_, unless, when)
import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Text (pack)
import Foreign.C (CInt (CInt))
import Game
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
import SDL.Event (pollEvents)
import qualified SDL.Font as TTF
import SDL.Input.Keyboard.Codes
import SDL.Vect
import SDL.Video
import System.Exit (exitSuccess)

type Bank a = Array Int a

type RenderArea = Rectangle CInt

gameWindowX :: CInt
gameWindowX = 640

gameWindowY :: CInt
gameWindowY = 800

gameAreaPadding :: CInt
gameAreaPadding = 12

gameGridX :: CInt
gameGridX = gameWindowX `div` 18

gameGridY :: CInt
gameGridY = gameWindowY `div` 5

gameGridW :: CInt
gameGridW = gameWindowX - 2 * gameGridX + gameAreaPadding

gameGridH :: CInt
gameGridH = gameWindowY - gameGridX - gameGridY + gameAreaPadding

gamePlayArea :: RenderArea
gamePlayArea = Rectangle (P $ V2 x y) (V2 w h)
  where
    x = gameGridX - gameAreaPadding
    y = gameGridY - gameAreaPadding
    w = gameGridW + gameAreaPadding
    h = gameGridH + gameAreaPadding

gameScoreArea :: Bank RenderArea
gameScoreArea =
  Array.listArray
    (1, numSections)
    [ let x = gameGridW + gameGridX - (w * fromIntegral i)
          y = gameGridX - gameAreaPadding
          w = (7 * (gameGridW `div` 16)) `div` fromIntegral numSections
          h = gameGridY - gameGridX - 2 * gameAreaPadding
       in Rectangle (P $ V2 x y) (V2 w h)
      | i <- [1 .. numSections]
    ]
  where
    numSections = 6

gameTiles :: Bank RenderArea
gameTiles =
  let tileX = gameGridW `div` 4
      tileY = gameGridH `div` 4
   in Array.listArray
        (1, 16)
        [ let rX = tileX * (i - 1) + gameGridX
              rY = tileY * (j - 1) + gameGridY
              rW = tileX - gameAreaPadding
              rH = tileY - gameAreaPadding
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

stringTexture :: TTF.Font -> Colors.RGB -> Renderer -> String -> IO Texture
stringTexture font color renderer str = do
  fontSurface <- TTF.solid font color (pack str)

  fontTexture <-
    createTextureFromSurface renderer fontSurface

  -- need to make sure the texture is eval'd before free
  fontTexture `seq` freeSurface fontSurface

  return fontTexture

tileTexture :: TTF.Font -> Renderer -> Tile -> IO Texture
tileTexture font renderer =
  -- TODO: resolve tile font color from function
  stringTexture font (Colors.rgb "#000000") renderer . show . tileValue

preloadTiles :: TTF.Font -> Renderer -> Int -> IO (Bank Texture)
preloadTiles font renderer count =
  Array.listArray (1, count)
    <$> sequence
      [ tileTexture font renderer $ createTile tileNo
        | tileNo <- [1 .. count]
      ]

preloadDigits :: TTF.Font -> Renderer -> IO (Bank Texture)
preloadDigits font renderer =
  Array.listArray (0, 9)
    <$> sequence
      [ stringTexture font (Colors.rgb "#000000") renderer (show digit)
        | digit <- [0 .. 9] :: [Int]
      ]

renderScore :: Integer -> Bank Texture -> Renderer -> IO ()
renderScore score digitBank renderer =
  let helper :: Int -> Integer -> IO ()
      helper idx remScore
        | remScore == 0 = return ()
        | not $ Array.inRange (Array.bounds gameScoreArea) idx = return ()
        | otherwise = do
            let slot = Just $ gameScoreArea ! idx
                (rest, k) = divMod remScore 10
                digit = fromIntegral k

            copy renderer (digitBank ! digit) Nothing slot
            helper (idx + 1) rest
   in do
        let (start, _) = Array.bounds gameScoreArea

        forM_
          gameScoreArea
          ( \scoreArea -> do
              rendererDrawColor renderer $= Colors.gameBG
              fillRect renderer (Just scoreArea)
          )

        when (score == 0) $ do
          copy renderer (digitBank ! 0) Nothing (Just $ gameScoreArea ! start)
          return ()

        helper start score

renderTile ::
  Bank Texture ->
  Maybe Tile ->
  RenderArea ->
  Renderer ->
  TTF.Font ->
  IO ()
renderTile tileBank maybeTile rectangle renderer font = do
  rendererDrawColor renderer $= Colors.tileColor maybeTile

  fillRect renderer (Just rectangle)

  case maybeTile of
    Nothing -> return ()
    Just tile -> do
      let idx = tileExponent tile

      useTexture <-
        if Array.inRange (Array.bounds tileBank) idx
          then pure $ tileBank ! idx -- ahead of time render
          else tileTexture font renderer tile -- jit render (large values)

      -- render the texture to the current rectangle
      copy renderer useTexture Nothing (Just rectangle)

renderGame ::
  Bank Texture ->
  Bank Texture ->
  Game ->
  Renderer ->
  TTF.Font ->
  IO ()
renderGame digitBank tileBank game renderer font = do
  let board = gameBoard game
      score = gameScore game

  rendererDrawColor renderer $= Colors.mainBG
  clear renderer

  rendererDrawColor renderer $= Colors.gameBG
  fillRect renderer (Just gamePlayArea)

  renderScore score digitBank renderer

  forM_
    (Array.indices board)
    ( \boardIdx@(row, col) ->
        -- board idxs are (1, 1), (1, 2), ... (4, 4); so we need to col - 1
        let rectIdx = 4 * (col - 1) + row
            curTile = board ! boardIdx
            curRect = gameTiles ! rectIdx
         in renderTile tileBank curTile curRect renderer font
    )

gameWindowLoop :: Game -> Renderer -> Window -> IO ()
gameWindowLoop game renderer window = do
  font <- TTF.load "/usr/share/fonts/TTF/JetBrainsMono-Regular.ttf" 12 -- FIXME:
  tileBank <- preloadTiles font renderer 14
  digitBank <- preloadDigits font renderer

  let loop loopGame loopRenderer = do
        maybeGame <- handleEvent loopGame

        case maybeGame of
          Nothing -> return ()
          Just newGame -> do
            renderGame digitBank tileBank newGame loopRenderer font

            present loopRenderer

            unless (isGameOver newGame || isGameSolved newGame) $ do
              loop newGame loopRenderer

  loop game renderer

  destroyWindow window
  TTF.free font
  TTF.quit
  exitSuccess

handleEvent :: Game -> IO (Maybe Game)
handleEvent game =
  flip handleGameEvent game <$> waitEvent

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

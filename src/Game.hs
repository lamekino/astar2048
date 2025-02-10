module Game
  ( Game (gameRng, gameScore, gameBoard),
    Move (North, South, East, West),
    GameResult (Solved, GameOver, Working),
    Tile (tileExponent, tileMerged),
    tileValue,
    createTile,
    createGame,
    isGameOver,
    isGameSolved,
    moveGame,
  )
where

import Control.Monad (forM_, when)
import Data.Array (Array, assocs, bounds, inRange, indices, listArray, (!))
import Data.Array.MArray (thaw, writeArray)
import Data.Array.ST (runSTArray)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bits (Bits (shiftL))
import Data.Maybe (isJust, isNothing)
import System.Random (Random (randomR), StdGen)
import Text.Printf (printf)

type Index = (Int, Int)

type TileExponent = Int

data Tile = Tile
  { tileMerged :: Bool,
    tileExponent :: TileExponent
  }
  deriving (Show, Eq)

type Board = Array Index (Maybe Tile)

data Move
  = North
  | South
  | East
  | West
  deriving (Show, Eq)

data Game = Game
  { gameRng :: StdGen,
    gameScore :: Integer,
    gameBoard :: Board
  }

instance Eq Game where
  g1 /= g2 = not $ g1 == g2
  g1 == g2 =
    let b1 = gameBoard g1
        b2 = gameBoard g2
     in all (\i -> b1 ! i == b2 ! i) $ indices b1

instance Show Game where
  show (Game _ score board) =
    let resolveTile Nothing = "[    ]"
        resolveTile (Just tile) =
          printf "[%4d]" $
            tileValue tile
     in unlines $
          printf "score: %d" score
            : [ concat
                  [resolveTile $ board ! (i, j) | j <- [1 .. 4]]
                | i <- [1 .. 4]
              ]

data GameResult
  = Solved Game
  | GameOver Game
  | Working Game

instance Show GameResult where
  show (Solved game) = "You Won! " ++ show game
  show (GameOver game) = "GAME OVER " ++ show game
  show (Working game) = show game

createTile :: Int -> Tile
createTile value = Tile {tileMerged = False, tileExponent = value}

tileValue :: Tile -> Integer
tileValue = shiftL 1 . tileExponent

createGame :: StdGen -> Game
createGame g = iterate addRandomTile emptyGame !! 2
  where
    emptyGame =
      Game
        { gameRng = g,
          gameScore = 0,
          gameBoard = listArray ((1, 1), (4, 4)) (repeat Nothing)
        }

isGameOver :: Game -> Bool
isGameOver (Game _ _ board) =
  let neighborIdxs op =
        concat
          [ zip
              (zipWith (op (,)) [1 .. 3] (repeat y))
              (zipWith (op (,)) [2 .. 4] (repeat y))
            | y <- [1 .. 4]
          ]
   in all isJust board
        && (not . any matchingTiles) (neighborIdxs flip)
        && (not . any matchingTiles) (neighborIdxs id)
  where
    matchingTiles :: (Index, Index) -> Bool
    matchingTiles (i1, i2) = board ! i1 == board ! i2

isGameSolved :: Game -> Bool
isGameSolved _ = False

pendingTiles :: Move -> Board -> [(Tile, Index)]
pendingTiles move board = [(v, k) | (k, Just v) <- (,) <*> (board !) <$> ks]
  where
    ks = [(i, j) | i <- fi [1 .. 4], j <- fj [1 .. 4]]
    fi = if move == South then reverse else id
    fj = if move == West then reverse else id

findFurthest :: Move -> Board -> Tile -> Index -> Index
findFurthest move board value idx =
  let traversal =
        takeWhile hasNext . drop 1 . iterate (bimap (+ dx) (+ dy)) $ idx
   in case traversal of
        [] -> idx
        xs -> last xs
  where
    (dx, dy) =
      case move of
        North -> (-1, 0)
        South -> (1, 0)
        East -> (0, -1)
        West -> (0, 1)

    hasNext :: Index -> Bool
    hasNext i =
      inRange (bounds board) i
        && (isNothing (board ! i) || Just value == board ! i)

moveTile :: Game -> Tile -> Index -> Index -> Game
moveTile game@(Game _ curScore curBoard) tile start finish =
  let hasMoved = start /= finish
      hasMerged = hasMoved && isJust (curBoard ! finish)

      nextExp =
        tileExponent tile + fromEnum hasMerged

      nextScore =
        if hasMerged then curScore + tileValue tile else curScore

      nextBoard = runSTArray $ do
        stBoard <- thaw curBoard

        writeArray stBoard finish . Just $
          Tile {tileMerged = hasMerged, tileExponent = nextExp}

        when hasMoved $ do
          writeArray stBoard start Nothing

        return stBoard
   in game {gameScore = nextScore, gameBoard = nextBoard}

removeMergeInfo :: Game -> Game
removeMergeInfo game@(Game _ _ curBoard) =
  let nextBoard = runSTArray $ do
        stBoard <- thaw curBoard

        forM_
          [(idx, tile) | (idx, Just tile) <- assocs curBoard]
          ( \(idx, tile) ->
              when (tileMerged tile) $ do
                writeArray stBoard idx (Just $ tile {tileMerged = False})
          )

        return stBoard
   in game {gameBoard = nextBoard}

addRandomTile :: Game -> Game
addRandomTile game@(Game g _ board) =
  let available = [idx | (idx, Nothing) <- assocs board]
      (pickIdx, g1) = randomR (0, length available - 1) g
      (value, g') = randomR (1, 2) g1

      nextBoard =
        runSTArray $ do
          stBoard <- thaw board

          when (available /= []) $ do
            writeArray
              stBoard
              (available !! pickIdx)
              (Just $ createTile value)

          return stBoard
   in game {gameRng = g', gameBoard = nextBoard}

moveGame :: Move -> Game -> Game
moveGame input game@(Game _ _ board) =
  updateTiles (pendingTiles input board) game
  where
    updateTiles :: [(Tile, Index)] -> Game -> Game
    updateTiles updates curGame@(Game _ _ curBoard) =
      case updates of
        [] ->
          if game == curGame
            then removeMergeInfo curGame
            else addRandomTile $ removeMergeInfo curGame
        ((tile, idx) : rest) ->
          let nextIdx = findFurthest input curBoard tile idx
              nextGame = moveTile curGame tile idx nextIdx
           in updateTiles rest nextGame

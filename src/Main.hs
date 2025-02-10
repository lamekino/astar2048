import Game (createGame)
import System.Random (getStdGen, mkStdGen)
import Window (gameWindowInit, gameWindowLoop)

main :: IO ()
main = do
  -- rng <- getStdGen
  let rng = mkStdGen 0
  uncurry (gameWindowLoop $ createGame rng) =<< gameWindowInit

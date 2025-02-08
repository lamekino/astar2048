import Game (createGame)
import System.Random (getStdGen, mkStdGen)
import Window (gameWindowInit, gameWindowLoop)

main :: IO ()
main = do
  rng <- pure $ mkStdGen 0 -- change to getStdGen to have true random numbers
  uncurry (gameWindowLoop $ createGame rng) =<< gameWindowInit

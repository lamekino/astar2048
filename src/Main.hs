import Game (createGame)
import System.Random (getStdGen, mkStdGen)
import Window (windowInit, windowLoop)

main :: IO ()
main = do
  -- rng <- getStdGen
  let rng = mkStdGen 0
  uncurry (windowLoop $ createGame rng) =<< windowInit

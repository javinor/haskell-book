module Ch26_14_Exercise_Morra where

import Control.Monad.IO.Class
import Control.Monad.State
import System.Random (randomRIO)

data Winner = 
    Player 
  | Computer 
  deriving (Eq, Show)

data Scores =
  Scores {
    player :: Int
  , computer :: Int
  }

updateScore :: Winner -> Scores -> Scores
updateScore winner (Scores p c) =
  case winner of
    Player -> Scores (p + 1) c
    Computer -> Scores p (c + 1)

gameOver :: Scores -> Maybe Winner
gameOver (Scores p c) = 
  case (p >= 3, c >= 3) of
    (True, _) -> Just Player
    (_, True) -> Just Computer
    _ -> Nothing

gameRound :: IO Winner
gameRound = do
  putStrLn $ "Please choose a number between 1 and 5: "
  pGuess <- read <$> getLine :: IO Int
  cGuess <- liftIO $ randomRIO (1, 5)
  
  let winner =
        if even (pGuess + cGuess)
        then Computer
        else Player

  putStrLn $ "P: " ++ show pGuess
  putStrLn $ "C: " ++ show cGuess
  putStrLn $ "- " ++ show winner ++ " wins the round"
  putStrLn ""
  return winner

game :: StateT Scores IO ()
game = do
  scores <- get
  
  case gameOver scores of
    Just p ->
      liftIO $ putStrLn $ "The winner is " ++ show p
    Nothing -> do
      winner <- liftIO gameRound
      modify (updateScore winner)
      game

main :: IO ()
main = do
  putStrLn "-- P is Player"
  putStrLn "-- C is Computer"
  putStrLn "-- Player is odds, Computer is evens."
  putStrLn ""

  _ <- runStateT game (Scores 0 0)
  return ()
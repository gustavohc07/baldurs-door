{-# LANGUAGE OverloadedStrings, NamedFieldPuns, TypeApplications, LambdaCase #-}

module Main where

import Data.Text as T
import qualified GHC.Base as T
import Control.Monad ( join )
import System.Random.Stateful ( uniformRM, globalStdGen )
import Data.List.NonEmpty

-- AC
-- Roll > AC
-- Roll for damage
-- Subtract the thing

main :: IO ()
main = do
  let initialState = GameState { playerHp = 100, monsterHp = 80, fstRound = True }
  -- join (afterFight <$> fightLoop initialState)
  putStrLn . renderFightResult =<< fightLoop initialState

data GameState = GameState { playerHp :: Int, monsterHp :: Int, fstRound :: Bool } deriving (Show)

data FightResult = Win | Died | Fighting

data PlayerCommand = Attack | Flee

parsePlayerCommand :: Text -> Maybe PlayerCommand
parsePlayerCommand command =
  case T.toUpper command of
    "A" ->
      Just Attack
    "F" ->
      Just Flee
    _ ->
      Nothing

getPlayerCommand :: IO Text
getPlayerCommand =
  T.pack <$> getLine

monsterHealth :: GameState -> Int
monsterHealth =
  monsterHp

playerHealth :: GameState -> Int
playerHealth =
  playerHp

roundMessage :: Bool -> String
roundMessage fstRound =
  if fstRound then
    "You just found a giant rat and it is ready to attack you! What would you do? (A)ttack, (F)lee?"
  else
    "The Rat is still alive! You are weak... Should you (A)ttack or (F)lee?"

fightStep :: GameState -> IO (GameState, FightResult)
fightStep g@GameState { playerHp, monsterHp, fstRound } =
  putStrLn (roundMessage fstRound) *>
  do
    -- putStrLn . roundMessage $ fstRound -> I could move line above into here
    mPlayerCommand <- parsePlayerCommand <$> getPlayerCommand
    case mPlayerCommand of
      Just Attack -> do
        -- Printing on Screen

        -- Generating random things
        playerAttackDamage <- rollDie D4
        monsterAttackDamage <- rollDie D8

        let monsterHp' = monsterHp - playerAttackDamage
        let playerHp' = playerHp - monsterAttackDamage

        -- Printing
        putStrLn ("You hit " ++ show playerAttackDamage ++ "!")
        putStrLn ("Rat hit you " ++ show monsterAttackDamage ++ "!")
        putStrLn ("Giant Rat HP: " ++ show monsterHp' ++ " VS. " ++ "Player HP: " ++ show playerHp')

        -- Refactor this out into a new function
        pure $ if monsterHp' > 0 && playerHp' > 0 then
          (g { monsterHp = monsterHp', playerHp = playerHp', fstRound = False }, Fighting)
        else if monsterHp' <= 0 && playerHp' > 0 then
          (g { monsterHp = monsterHp', fstRound = False }, Win)
        else
          (g, Died)
      Just Flee -> do
        putStrLn "Coward. While running you steped into a rock, fell, banged your head, and died."
        pure (g { playerHp = 0, fstRound = False }, Died)
      Nothing -> do
        putStrLn "What is this non-sense?"
        fightStep g


data DiceType = D4 | D8 | D20

rollDie :: DiceType -> IO Int
rollDie = \case
  D4 ->
    diceRoll 4
  D8 ->
    diceRoll 8
  D20 ->
    diceRoll 20
  where
    diceRoll maxRoll = uniformRM (1, maxRoll) globalStdGen


-- <$> :: (a -> b) -> k a -> k b
--        (DiceType -> IO Int) -> NonEmpty DiceType -> NonEmpty (IO Int)
rollDice :: NonEmpty DiceType -> IO (NonEmpty Int)
rollDice = traverse rollDie
-- rollDice d = sequenceA (rollDie <$> d)


-- Advantange Roll
-- Disadiventage Roll - Take the

fightLoop :: GameState -> IO FightResult
fightLoop gs = do
  (gs', fr) <- fightStep gs
  case fr of
    Win ->
      pure fr
    Died ->
      pure fr
    Fighting ->
      fightLoop gs'

renderFightResult :: FightResult -> String
renderFightResult = \case
    Win ->
      "You win!"

    Died ->
      "You got yourself kicked in the arse by a rat. What a loser."

    Fighting ->
      "This should not happen. Blame the developer"


-- USE THIS

attackResult :: IO ()
attackResult = undefined

data AttackOutcome = CriticalSuccess | CriticalMiss | RegularAttack | Miss

attackRoll :: Int -> Int -> AttackOutcome
attackRoll monsterAc roll
  | roll == 20 = CriticalSuccess
  | roll == 1 = CriticalMiss
  | roll >= monsterAc = RegularAttack
  | otherwise = Miss

-- IO FightResult -> FightResult -> IO ()
{-
fightLoop' gs =
  fightStep gs >>= \(gs', fr) ->
  case fr of
    ...
-}







-- List Comprehension might be useful
-- [ x+y | (x,y) <- List of Tuples - [(1,2)] ]


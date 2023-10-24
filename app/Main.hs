{-# LANGUAGE OverloadedStrings, NamedFieldPuns, LambdaCase #-}

module Main where

import Data.Text as T
import qualified GHC.Base as T
import Control.Monad ( join )
import System.Random.Stateful ( uniformRM, globalStdGen )
import Data.List.NonEmpty

main :: IO ()
main = do
  putStrLn . renderFightResult =<< fightLoop initialGameState

data GameState = GameState { player :: Player, monster :: Monster, fstRound :: Bool } deriving (Show)

data FightResult = Win | Died | Fighting

data PlayerCommand = Attack | Flee

-- Monster have stat block - which is the statistics of a monster.
    -- Type
    -- Armor Class
    -- Attributes
    -- Skills
    -- Hp
    -- Challenge (amount of exp and how hard is the monster)

data Monster = Monster { monsterHp :: Int, monsterArmorClass :: Int } deriving (Show)

data Player = Player { playerHp :: Int, playerArmorClass :: Int } deriving (Show)

initialGameState :: GameState
initialGameState = GameState { player = Player { playerHp = 100, playerArmorClass = 12 }, monster = Monster { monsterHp = 80, monsterArmorClass = 8 }, fstRound = True }

-- Fight Rendering

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

roundMessage :: Bool -> String
roundMessage fstRound =
  if fstRound then
    "You just found a giant rat and it is ready to attack you! What would you do? (A)ttack, (F)lee?"
  else
    "The Rat is still alive! You are weak... Should you (A)ttack or (F)lee?"

fightStep :: GameState -> IO (GameState, FightResult)
fightStep g@GameState { player, monster, fstRound } =
  putStrLn (roundMessage fstRound) *>
  do
    mPlayerCommand <- parsePlayerCommand <$> getPlayerCommand
    case mPlayerCommand of
      Just Attack -> do
        -- Generating random numbers (rolls, attacks)
        playerAttackRoll <- rollDie D20
        monsterAttackRoll <- rollDie D20
        playerAttackDamage <- damageRoll (attackRoll (monsterArmorClass monster) playerAttackRoll)
        monsterAttackDamage <- damageRoll (attackRoll (playerArmorClass player) monsterAttackRoll)

        let monsterHp' = monsterHp monster - playerAttackDamage
        let playerHp' = playerHp player - monsterAttackDamage

        -- Printing
        putStrLn ("You hit " ++ show playerAttackDamage ++ "!")
        putStrLn ("Rat hit you " ++ show monsterAttackDamage ++ "!")
        putStrLn ("Giant Rat HP: " ++ show monsterHp' ++ " VS. " ++ "Player HP: " ++ show playerHp' ++ "\n")

        pure $ someFnName g monsterHp' playerHp'
      Just Flee -> do
        putStrLn "Coward. While running you steped into a rock, fell, banged your head, and died."
        pure (g { player = player {playerHp = 0} , fstRound = False }, Died)
      Nothing -> do
        putStrLn "What is this non-sense?"
        fightStep g


someFnName :: GameState -> Int -> Int -> (GameState, FightResult)
someFnName g monsterHp playerHp
  | monsterHp > 0 && playerHp > 0 =
    (g { monster = (monster g) { monsterHp = monsterHp }, player = (player g) { playerHp = playerHp }, fstRound = False }, Fighting)
  | monsterHp <= 0 && playerHp > 0 =
    (g { monster = (monster g) { monsterHp = monsterHp }, fstRound = False }, Win)
  | otherwise = (g, Died)


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


-- Die Roll

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


rollDice :: NonEmpty DiceType -> IO (NonEmpty Int)
rollDice = traverse rollDie

-- Advantange Roll - Take the highest roll
-- Disadvantage Roll - Take the lowest roll

-- Attack

damageRoll :: AttackOutcome -> IO Int
damageRoll = \case
  CriticalSuccess ->
    -- We are rolling D20, normally it rolls the weapon damage.
    rollDie D20
  RegularAttack ->
    rollDie D20
  Miss ->
    pure 0
  CriticalFailure ->
    pure 0

data AttackOutcome = CriticalSuccess | CriticalFailure | RegularAttack | Miss

attackRoll :: Int -> Int -> AttackOutcome
attackRoll targetAc roll
  | roll == 20 = CriticalSuccess
  | roll == 1 = CriticalFailure
  | roll >= targetAc = RegularAttack
  | otherwise = Miss


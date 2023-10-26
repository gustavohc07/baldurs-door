{-# LANGUAGE OverloadedStrings, NamedFieldPuns, LambdaCase, InstanceSigs, RecordWildCards #-}

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

-- Map
  -- Generate monster and state of monsters there
  -- Exploration
  -- Generate monster based on the challenging rating
  --

-- Monster have stat block - which is the statistics of a monster.
    -- Type
    -- Armor Class
    -- Size
      -- data Size = Tiny | Small | Medium | Large | Huge | Gargantuan deriving (Show)
    -- Hp
    -- Attributes
    -- Skills
    -- Challenge (amount of exp and how hard is the monster)
    -- isLegendary :: Bool

data Monster = Monster { name :: Text, statBlock :: StatBlock } deriving (Show)

data StatBlock = StatBlock { hp :: Int, armorClass :: Int } deriving (Show)

data Player = Player { playerHp :: Int, playerArmorClass :: Int } deriving (Show)

-- --
newtype State s a =
  State (
    s
    -> (a, s)
  )

runState ::
  State s a
  -> s
  -> (a, s)
runState (State f) =
  f

get ::
  State s s
get =
  State (\a -> (a, a))

put ::
  s
  -> State s ()
put state =
  State (\_ -> ((), state))


instance Functor (State s) where
  fmap ::
    (a -> b)
    -> State s a
    -> State s b
  fmap f (State fn) =
    State $ \s0 ->
      let (a, s1) = fn s0
          b = f a
      in
      (b, s1)

instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure a =
    State $ \s -> (a, s)
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b
  (<*>) (State sab) (State sa)  =
    State $ \s0 -> let
      (a2b, s1) = sab s0
      (a, s2) = sa s1
      in
      (a2b a, s2)


instance Monad (State s) where
  (>>=) ::
    State s a
    -> (a -> State s b)
    -> State s b
  (>>=) (State sa) assb =
    State $ \s0 -> let
      (a, s1) = sa s0
      ssb = assb a
      in
      runState ssb s1

-- incrementNum :: State Int Int
-- incrementNum = do
--   x <- get
--   put $ x + 1
--   y <- get
--   pure y

-- runIncrementNum :: (Int, Int)
-- runIncrementNum =
--   runState incrementNum 20
-- --

initialGameState :: GameState
initialGameState = GameState {
  player = Player { playerHp = 100, playerArmorClass = 12 }
, monster = Monster { name = "Rat", statBlock = initialStatBlock }
, fstRound = True }

initialStatBlock :: StatBlock
initialStatBlock = StatBlock { hp = 80, armorClass = 8 }

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

roundMessage :: Text -> Bool -> String
roundMessage monsterName fstRound =
  if fstRound then
    "You just found a " ++ T.unpack monsterName ++ " and it is ready to attack you! What would you do? (A)ttack, (F)lee?"
  else
    "The " ++ T.unpack monsterName ++ " is still alive! You are weak... Should you (A)ttack or (F)lee?"

monsterHp :: Monster -> Int
monsterHp monster =
  hp (statBlock monster)

monsterAc :: Monster -> Int
monsterAc monster =
  armorClass (statBlock monster)

fightStep :: GameState -> IO (FightResult, GameState) -- StateT
fightStep g@GameState { player, monster, fstRound } =
  putStrLn (roundMessage (name monster) fstRound) *>
  do
    mPlayerCommand <- parsePlayerCommand <$> getPlayerCommand
    case mPlayerCommand of
      Just Attack -> do
        -- Generating random numbers (rolls, attacks)
        playerAttackRoll <- rollDie D20
        monsterAttackRoll <- rollDie D20
        playerDamageRoll <- damageRoll (attackRoll (monsterAc monster) playerAttackRoll)
        monsterDamageRoll <- damageRoll (attackRoll (playerArmorClass player) monsterAttackRoll)

        let monsterHp' = monsterHp monster - playerDamageRoll
        let playerHp' = playerHp player - monsterDamageRoll

        -- Printing
        putStrLn ("You hit " ++ show playerDamageRoll ++ "!")
        putStrLn ("Rat hit you " ++ show monsterDamageRoll ++ "!")
        putStrLn ("Giant Rat HP: " ++ show monsterHp' ++ " VS. " ++ "Player HP: " ++ show playerHp' ++ "\n")

        pure $ runState (computeBattleState' monsterHp' playerHp') g
        -- IO (State GameState FightResult)
      Just Flee -> do
        putStrLn "Coward. While running you steped into a rock, fell, banged your head, and died."
        pure $ flip runState g $ do
          GameState {..} <- get
          let updateState = GameState { player = player {playerHp = 0} , fstRound = False, ..}
          put updateState
          pure Died

      Nothing -> do
        putStrLn "What is this non-sense?"
        fightStep g


computeBattleState :: GameState -> Int -> Int -> (GameState, FightResult)
computeBattleState g@GameState { player, monster } monsterHp playerHp
  | monsterHp > 0 && playerHp > 0 =
    (g { monster = monster { statBlock = (statBlock monster) { hp = monsterHp } }, player = player { playerHp = playerHp }, fstRound = False }, Fighting)
  | monsterHp <= 0 && playerHp > 0 =
    (g { monster =  monster { statBlock = (statBlock monster) { hp = monsterHp } }, fstRound = False }, Win)
  | otherwise = (g, Died)

                                  -- State s a -> (s, a)
computeBattleState' :: Int -> Int -> State GameState FightResult
computeBattleState' monsterHp playerHp
  | monsterHp > 0 && playerHp > 0 = do
      GameState {..} <- get
      let updatedState = GameState { monster = monster { statBlock = (statBlock monster) { hp = monsterHp } }, player = player { playerHp = playerHp }, fstRound = False, ..}
      put updatedState
      pure Fighting -- WHERE
  | monsterHp <= 0 && playerHp > 0 = do
      GameState {..} <- get
      let updateState = GameState { monster =  monster { statBlock = (statBlock monster) { hp = monsterHp } }, fstRound = False, ..}
      put updateState
      pure Win
  | otherwise = pure Died

fightLoop :: GameState -> IO FightResult
fightLoop gs = do
  (fr, gs') <- fightStep gs
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
    -- We are rolling D20, normally it rolls the weapon damage
    rollDie D20
  RegularAttack ->
    rollDie D20
  Miss ->
    pure 0
  CriticalFailure ->
    pure 0

data AttackOutcome = CriticalSuccess | CriticalFailure | RegularAttack | Miss deriving Show

attackRoll :: Int -> Int -> AttackOutcome
attackRoll targetAc roll
  | roll == 20 = CriticalSuccess
  | roll == 1 = CriticalFailure
  | roll >= targetAc = RegularAttack
  | otherwise = Miss


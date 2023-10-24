{- Chapter 3 task 9 -}
module Fight where

class Fighter a where
  fight :: a -> a -> a
  isDead :: a -> Bool

data Character = NewCharacter
  { characterHealth   :: Health
  , characterAttack   :: Attack
  , characterStrength :: Strength
  , characterDefense  :: Maybe Defense
  , characterSkills    :: [Skill]
  } deriving Show

newtype Health = Health {getHealth :: Int} deriving (Eq, Show)
newtype Attack = Attack {getAttack :: Int} deriving Show
newtype Strength = Strength {getStrength :: Int} deriving Show
newtype Damage = Damage {getDamage :: Int} deriving Show
newtype Defense = Defense {getDefense :: Int} deriving Show
data Skill = Hit | DrinkPotion | CastBarrier | RunAway deriving (Eq, Ord, Show)

calculateDamage :: Attack -> Strength -> Damage
calculateDamage (Attack attack) (Strength strength) = Damage (attack + strength)

calculateInflictedDamage :: Health -> Maybe Defense -> Damage -> Health
calculateInflictedDamage (Health health) (Just (Defense defense)) (Damage damage) = Health $ health - max 0 (damage - defense)
calculateInflictedDamage (Health health) Nothing (Damage damage) = Health $ health - damage

restoreHealth :: Health -> Int -> Health
restoreHealth (Health 200) _ = Health 200
restoreHealth (Health health) amount
  | healthCoefficient == incrementedHealth = Health incrementedHealth
  | otherwise = Health (incrementedHealth - healthCoefficient)
  where incrementedHealth = health + amount
        healthCoefficient = mod incrementedHealth 200

incrementArmor :: Maybe Defense -> Maybe Defense
incrementArmor Nothing = Just $ Defense 4
incrementArmor (Just (Defense defense)) 
  | armorCoefficient == incrementedArmor = Just $ Defense incrementedArmor
  | otherwise = Just $ Defense $ incrementedArmor - armorCoefficient
  where incrementedArmor = defense + 4
        armorCoefficient = mod incrementedArmor 30

hit :: Character -> Character -> Character
hit c1 c2 = c2 {characterHealth = calculateInflictedDamage  targetHealth targetDefense attackerDamage}
  where targetHealth = characterHealth c2
        targetDefense = characterDefense c2
        attackerDamage = calculateDamage (characterAttack c1) (characterStrength c1)

drinkPotion :: Character -> Character
drinkPotion knight = knight {characterHealth = restoreHealth  (characterHealth knight) 50}

castBarrier :: Character -> Character
castBarrier knight = knight {characterDefense = incrementArmor (characterDefense knight)}

updateSkills :: Character -> Character
updateSkills character = character {characterSkills = discardSkill (characterSkills character)}
  where discardSkill :: [Skill] -> [Skill]
        discardSkill [] = []
        discardSkill (x:xs) = xs ++ [x]

turn :: ([Skill], Character, Character) -> ([Skill], Character, Character)
turn (Hit:xs, p1, p2) = (xs, updateSkills p1, hit p1 p2)
turn (DrinkPotion:xs, p1, p2) = (xs, updateSkills $ drinkPotion p1, p2)
turn (CastBarrier:xs, p1, p2) = (xs, updateSkills $ castBarrier p1,  p2)
turn (RunAway:xs, p1, p2) = (xs, updateSkills p1, p2)
turn ([], p1, p2) = turn (characterSkills p1, p1, p2)


instance Fighter Character where

  isDead :: Character -> Bool
  isDead = (<= 0) . getHealth . characterHealth

  fight :: Character -> Character -> Character
  fight fighter1 fighter2 
    | isDead fighter1 = fighter2
    | isDead fighter2 = fighter1
    | otherwise = fight p2After p1
    where (_, p1, p2After) = turn ([], fighter1, fighter2)


-- For testing in the REPL
player1 = NewCharacter (Health 200) (Attack 25) (Strength 10) (Just (Defense 5)) [Hit, DrinkPotion, CastBarrier]
player2 = NewCharacter (Health 300) (Attack 35) (Strength 20) Nothing [Hit, RunAway]
player3 = NewCharacter (Health 350) (Attack 25) (Strength 30) (Just (Defense 5)) [Hit, Hit, Hit, CastBarrier]
player4 = NewCharacter (Health 500) (Attack 45) (Strength 20) Nothing [Hit, RunAway]
player5 = NewCharacter (Health 0) (Attack 45) (Strength 20) Nothing [Hit, RunAway]

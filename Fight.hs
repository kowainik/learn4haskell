module Fight where

class Fighter a where
  fight :: a -> a -> a

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
incrementArmor (Just (Defense defense)) = Just $ Defense (defense + 10)
incrementArmor Nothing = Just $ Defense 10

hit :: Character -> Character -> Character
hit c1 c2 = c2 {characterHealth = calculateInflictedDamage  targetHealth targetDefense attackerDamage}
  where targetHealth = characterHealth c2
        targetDefense = characterDefense c2
        attackerDamage = calculateDamage (characterAttack c1) (characterStrength c1)

drinkPotion :: Character -> Character
drinkPotion knight = knight {characterHealth = restoreHealth  (characterHealth knight) 50}

castBarrier :: Character -> Character
castBarrier knight = knight {characterDefense = incrementArmor (characterDefense knight)}

turn :: ([Skill], Character, Character) -> ([Skill], Character, Character)
turn (Hit:xs, p1, p2) = (xs, p1, hit p1 p2)
turn (DrinkPotion:xs, p1, p2) = (xs, drinkPotion p1, p2)
turn (CastBarrier:xs, p1, p2) = (xs, castBarrier p1,  p2)
turn (RunAway:xs, p1, p2) = (xs, p1, p2)
turn ([], p1, p2) = (characterSkills p1, p1, p2)

player1 = NewCharacter (Health 200) (Attack 25) (Strength 10) (Just (Defense 10)) [Hit, DrinkPotion, CastBarrier]
player2 = NewCharacter (Health 300) (Attack 35) (Strength 20) (Nothing) [Hit, RunAway]


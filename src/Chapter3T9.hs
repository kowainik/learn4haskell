{-# LANGUAGE InstanceSigs #-}

{-
=💣= Task 9*

You got to the end of Chapter Three, Brave Warrior! I hope you are ready for the
final Boss. We talked a lot about data types and typeclasses in this chapter.
Now it is time to show what we learned!

To defeat the final boss in this chapter, you need to implement your own data
types, typeclasses and instances, describing the world, and write polymorphic
functions using custom types and typeclasses.

The task:
When two fighters engage in a battle (knight fights the monster, duel of
knights, monsters fighting for a lair, etc.), both of them can perform different
actions. They do their activities in turns, i.e. one fighter goes first, then
the other goes second, then the first again, and so on, until one of them wins.

Both knight and monster have a sequence of actions they can do. A knight can
attack, drink a health potion, cast a spell to increase their defence. A monster
can only attack or run away. Each fighter starts with some list of actions they
can do, performs them in sequence one after another, and when the sequence ends,
the process starts from the beginning.

Monsters have only health and attack, while knights also have a defence. So when
knights are attacked, their health is decreased less, if they have more defence.
The fight ends when the health of one fighter becomes zero or less.

As you can see, both monster and knight have similar characteristics, but they
also have some differences. So it is possible to describe their common
properties using typeclasses, but they are different data types in the end.

Implement data types and typeclasses, describing such a battle between two
contestants, and write a function that decides the outcome of a fight!
-}

-- knight [ health, attack, defense]
-- - attack
-- - drink health potion to increase health
-- - cast spell to incraese defense 

-- monster [ health, attack ] 
-- - attack 
-- - run 

data KnightAction = KnightAttack | KnightDrink Int | KnightCast Int deriving (Show)
data MonsterAction = MonsterAttack | MonsterRun deriving (Show)

newtype Action = KnightAction | MonsterAction

data Knight = Knight {
  knightHealth :: Int,
  knightAttack :: Int,
  knightDefense :: Int,
  knightActions :: [KnightAction]
} deriving (Show)

data Monster = Monster {
  monsterHealth :: Int,
  monsterAttack :: Int,
  monsterActions :: [MonsterAction]
} deriving (Show)

class Fighter a where
  takeHit :: a -> Int -> a
  performAction :: (Fighter b) => a -> b -> Action -> (a, b)

instance Fighter Knight where
  takeHit :: Knight -> Int -> Knight
  takeHit k@Knight {knightHealth=kh, knightAttack=_, 
                    knightDefense=kd, knightActions=_} damage =
      k { knightHealth = kh - max 0 (damage - kd) }

  performAction :: Knight -> a -> Action -> (Knight, a)
  performAction k enemy KnightAttack = 
    (k, takeHit enemy (knightAttack k))
  performAction k enemy (KnightDrink d) =
    (k { knightHealth = knightHealth k + d }, enemy)
  performAction k enemy (KnightCast c) = 
    (k { knightDefense = knightDefense k + c }, enemy)

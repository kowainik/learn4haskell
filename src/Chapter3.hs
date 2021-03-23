{- 👋 Welcome to Chapter Three of our journey, Courageous Knight!

Glad to see you back for more challenges. You fought great for the glory of the
Functional Programming in the previous Chapters. We are grateful that you are
continuing to walk this road with us.

This Chapter requires the knowledge from the previous modules, so it is highly
encouraged to at least look through the material of those chapters.

Let's refresh how the training works.

== You are on __Chapter Three__.

At this step, we are going to learn more about types in Haskell and explore
typeclasses. You will need to create a lot of types, so we rely on your
creativity, as you will be given the opportunity to create new worlds out of
Haskell code.

Specifically, in this chapter, you are going to practice:

 ✧ Types in Haskell
 ✧ ADTs: Algebraic Data Types
 ✧ Type aliases vs Data types vs Newtypes
 ✧ Parametric polymorphism
 ✧ Typeclasses
 ✧ Ad-hoc polymorphism

As usual, the explanations are in the Haskell comments of this module. We are
leaving a number of tasks on our path. Your goal is to solve them all.

After finishing the PR, you can choose to summon us, @vrom911 and @chshersh, to
look at your solution in order to give some advice on your code. This is
optional; however, you can ask us for review only if you want some feedback on
your solutions.

Okay. Ready? Set. Go!
-}

{-
=⚗️= Language extensions*

Some Haskell features are not enabled by default. They can be enabled by turning
on corresponding extensions (the language feature) using the LANGUAGE pragma at
the top of the file before the "module" declaration. The fact that they are not
enabled by default doesn't mean they are experimental. This is just the way
Haskell works.

In this module, we enable the "InstanceSigs" feature that allows writing type
signatures in places where you can't by default. We believe it's helpful to
provide more top-level type signatures, especially when learning Haskell.
-}
{-# LANGUAGE InstanceSigs, MultiParamTypeClasses #-}

module Chapter3 where
import Data.Maybe
{-
=🛡= Types in Haskell

Let's talk about types.
In the previous chapters, we have already worked a lot with them. And we bet,
you can name a few at this point.

But we mostly were operating on primitive types, like 'Int' or 'Bool'. We saw
the function (->) a lot! We briefly touched tuples, but we learned that lists
are far more complicated!

Haskell has several different ways to create entirely new data types. Let's talk
about them all and master our skill of data types construction.
-}

{- |
=🛡= Type aliases

The simplest way to introduce a new type in Haskell is __type aliases__. Type
aliases are nothing more than just other names to already existing types.
A few examples:

@
type IntList = [Int]
type IntPair = (Int, Int)
@

Type aliases are just syntactic sugar and would be replaced by the real types
during compilation, so they don't bring too much to the table of data types.
However, they may make the life of a developer easier in some places.

One of the most common type aliases is 'String' that we already mentioned in the
List section of the previous chapter. And it is defined in the following way:

@
type String = [Char]
@

Now it makes much more sense of why 'String' is related to lists. You can see
that any list function could be used on 'String's as well.

👩‍🔬 Due to the implementation details of lists, such representation of String
  is highly inefficient. It is unfortunate that the list of characters is the
  default String type. Experienced Haskellers use more efficient string types
  'Text' and 'ByteString' from Haskell libraries: 'text' and 'bytestring'
  correspondingly. But, for the simplicity of this training, we are using
  'String'.

Another common type alias in the standard library is "FilePath", which is the
same as 'String' (which is the same as '[Char]'):

@
type FilePath = String
@

Usually, you are encouraged to introduce new data types instead of using aliases
for existing types. But it is still good to know about type aliases. They have a
few use-cases, and you can meet them in various Haskell libraries as well.
-}

{- |
=🛡= ADT

Let's not limit ourselves with just type aliases and define some real new data
structures. Are we the creators of new worlds or what?

Type in Haskell is like a box of information description that the object of that
type should contain.

Haskell uses Algebraic Data Types (ADT) system of types. That means that there
are two types of types: product and sum types.

To give you some basic understanding of the difference between these two, let's
go to the book shop. A book in there represents a product type: each book has
the name, author, cover, pages, etc. And all of these properties are mandatory
and come with the book. Bookshelf, in its turn, is a sum type. Each book in a
shelf is a different type, and you can choose one of them at once (there is no
such book where two or more physical books are sewed together).

We will show you an example now, just to illustrate all the above and then will
explain each concept separately. Note, this is not real syntax.

@
-- Product type
Book:
     book name
 AND book author
 AND book cover
 AND book pages


-- Sum type
BookShelf:
    Good  book 1 : {Book}
 OR Good  book 2 : {Book}
 OR Cheap book 3 : {Book}
@

👩‍🔬 We use AND in product types to represent the notion of having all fields at
  the same time. In contrast, for the sum types, we use OR to tell only about a
  single possibility. AND in logic corresponds to multiplication in math, and OR
  corresponds to addition. You see that there is some math theory behind the
  concept of data types in Haskell, and that's why they are called Algebraic Data
  Types.
-}

{- |
=🛡= Product type

Let's now see how product data types look like in Haskell.

Product type should have a type name, one type constructor (the function that
lets you create the value of the type later) and the description of the fields
it consists of in the view of types.

When defining a custom type in Haskell, you use the __"data"__ keyword, write
the __type name__ you come up with after it, and then after the "=" sign you
specify the __constructor name__ followed by the __fields__.

When in action, a custom data type could be used in the following use case.
A definition of a data type for a knight with a name and number of victories
can look like this:

@
      ┌─ type name
      │
      │       ┌─ constructor name (or constructor tag)
      │       │
data Knight = MkKnight String Int
 │                       │    │
 │                       └────┴─── types of fields
 │
 └ "data" keyword
@

♫ NOTE: The constructor can have the same name as the type itself. And in most
  cases, they are indeed named identically. This is not a problem for Haskell,
  because types live in the types namespace, and constructors live in the value
  namespace. So there won't be any collisions and misunderstandings from the
  compiler side. The names are unambiguous.

You can use the constructor name, to create a value of the "Knight" type.

@
arthur :: Knight
arthur = MkKnight "Arthur" 100
@

A constructor is just a function from fields to the type! You can verify this in GHCi:

ghci> :t MkKnight
MkKnight :: String -> Int -> Knight

As in a regular function, you need to provide a 'String' and an 'Int' to the
'MkKnight' constructor in order to get the full-fledged 'Knight'.

Also, you can write a function that takes a Knight and returns its name.
It is convenient to use pattern matching for that:

@
knightName :: Knight -> String
knightName (MkKnight name _) = name
@

And you can extract the name in GHCi:

ghci> knightName arthur
"Arthur"

It is comfy to have such getters for all types, so Haskell provides a syntax for
defining __records__ — named parameters for the product data type fields.

Records have a similar syntax for defining in Haskell as (unnamed) ordinary
product types, but fields are specified in the {} separated by a comma. Each
field should have a name and a type in this form: 'fieldName :: FieldType'.

The same definition of 'Knight' but with records should be written in the
following way:

@
data Knight = MkKnight
    { knightName      :: String
    , knightVictories :: Int
    }
@

The above type definition is equivalent to the one we had before. We just gave
names to our fields for clarity. In addition, we also automatically get
functions "knightName :: Knight -> String" and "knightVictories :: Knight ->
Int". This is what records bring us — free getters!

The pattern matching on constructors of such records can stay the same. Besides,
you can use field names as getters.

👩‍🔬 We are using a particular naming scheme of record field names in record
  types in order to avoid name collisions. We add the data type name prefix in
  front of each usual field name for that. As we saw, records create getters for
  us, which are actually top-level functions. In Haskell, all functions defined at
  top-level are available in the whole scope within a module. But Haskell forbids
  creating multiple functions with the same name. The Haskell ecosystem has
  numerous ways of solving this so-called "record" problem. Still, for simplicity
  reasons, we are going to use disambiguation by a prefix which is one of the
  standard resolutions for the scope problem.

In addition to getting top-level getters automatically, you get the following
features for free when using records over unnamed type fields:

 1. Specify names during constructions — additional visual help for code.
 2. Record-update syntax.

By default, all functions and constructors work with positional arguments
(unnamed, that are identified by its position in the type declaration). You
write a function or a constructor name first, and then you pass arguments
separated by spaces. But once we declare a product type as a record, we can use
field names for specifying constructor values. That means that the position
doesn't matter anymore as long as we specify the names. So it is like using
named arguments but only for constructors with records.
This is an alternative way of defining values of custom records.

Let's introduce Sir Arthur properly!

@
arthur :: Knight
arthur = MkKnight
    { knightName = "Arthur"
    , knightVictories = 100
    }
@

After we created our custom types and defined some values, we may want to change
some fields of our values. But we can't actually change anything! Remember that
all values in Haskell are immutable, and you can't just change a field of some
type. You need to create a new value! Fortunately, for records, we can use the
__record update syntax__. Record update syntax allows creating new objects of a
record type by assigning new values to the fields of some existing record.

The syntax for record update is the following:

@
lancelot :: Knight
lancelot = arthur { knightName = "Lancelot" }
@

Without records, we had to write a custom setter function for each field of each
data type.

@
setKnightName :: String -> Knight -> Knight
setKnightName newName (MkKnight _ victories) =
    MkKnight newName victories
@

♫ NOTE: By default, GHCi doesn't know how to display values of custom types. If
  you want to explore custom data types in the REPL, you need to add a magical
  "deriving (Show)" line (will be explained later in this chapter) at the end of a
  record. Like so:

@
data Knight = MkKnight
    { knightName      :: String
    , knightVictories :: Int
    } deriving (Show)
@

Now GHCi should be able to show the values of your types! Try playing with our
knights in GHCi to get the idea behind records.

🕯 HINT: At this point, you may want to be able to enter multi-line strings in
  GHCi. Start multi-line blocks by typing the ":{" command, and close such blocks
  using the ":}" command.

ghci> :{
ghci| data Knight = MkKnight
ghci|     { knightName      :: String
ghci|     , knightVictories :: Int
ghci|     } deriving (Show)
ghci| :}
ghci>

Although, it may be easier to define data types in the module, and load it
afterwards.
-}

{- |
=⚔️= Task 1

Define the Book product data type. You can take inspiration from our description
of a book, but you are not limited only by the book properties we described.
Create your own book type of your dreams!
-}

data Book = MkBook
    { bookPublishYear   :: Int
    , bookTitle         :: String
    , bookAuthor        :: String
    } deriving (Show)


-- bible :: Book
-- bible = MkBook
--     { bookPublishYear = 0
--     , bookTitle = "Bible"
--     , bookAuthor = "Unknown"
--     } 


{- |
=⚔️= Task 2

Prepare to defend the honour of our kingdom! A monster attacks our brave knight.
Help him to fight this creature!

Define data types for Knights and Monsters, and write the "fight" function.

Both a knight and a monster have the following properties:

 ✦ Health (the number of health points)
 ✦ Attack (the number of attack units)
 ✦ Gold (the number of coins)

When a monster fights a knight, the knight hits first, and the monster hits back
only if it survives (health is bigger than zero). A hit decreases the amount of
health by the number represented in the "attack" field.

Implement the "fight" function, that takes a monster and a knight, performs the
fight following the above rules and returns the amount of gold the knight has
after the fight. The battle has the following possible outcomes:

 ⊛ Knight wins and takes the loot from the monster and adds it to their own
   earned treasure
 ⊛ Monster defeats the knight. In that case return -1
 ⊛ Neither the knight nor the monster wins. On such an occasion, the knight
   doesn't earn any money and keeps what they had before.

-}

data Fighter = MkFighter
    { fighterHealthPts    :: Int,
      fighterAttackPts    :: Int,
      fighterGold         :: Int
    } deriving (Show)

knight :: Fighter
knight = MkFighter
    { fighterHealthPts = 10,
      fighterAttackPts = 2,
      fighterGold = 5
    }

monster :: Fighter
monster = MkFighter
    { fighterHealthPts = 15,
      fighterAttackPts = 1,
      fighterGold = 11
    }


fight :: Fighter -> Fighter -> Int
fight kn mn 
    | fighterHealthPts kn < 1 = if fighterHealthPts mn < 1 then fighterGold kn else -1
    | fighterHealthPts mn < 1 = (fighterGold kn) + (fighterGold mn)
    | fighterAttackPts kn < 1 = if fighterAttackPts mn < 1 then fighterGold kn else -1
    | fighterAttackPts mn < 1 = (fighterGold kn) + (fighterGold mn)
    | otherwise = if knToDeathRounds kn mn >= mnToDeathRounds kn mn then (fighterGold kn) + (fighterGold mn) else -1
    where 
        knToDeathRounds :: Fighter -> Fighter -> Int
        knToDeathRounds kn mn = if (fighterHealthPts kn) `rem` (fighterAttackPts mn) > 0 then (fighterHealthPts kn) `div` (fighterAttackPts mn) + 1 else (fighterHealthPts kn) `div` (fighterAttackPts mn)
        mnToDeathRounds :: Fighter -> Fighter -> Int
        mnToDeathRounds kn mn = if (fighterHealthPts mn) `rem` (fighterAttackPts kn) > 0 then (fighterHealthPts mn) `div` (fighterAttackPts kn) + 1 else (fighterHealthPts mn) `div` (fighterAttackPts kn) 

    -- let mnHealthLeft = fighterHealthPts mn - fighterAttackPts kn
    -- in if mnHealthLeft > 0 then 
    --     let knHealthLeft = fighterHealthPts kn - fighterAttackPts mn
    --     in if knHealthLeft > 0 then fight (kn { fighterHealthPts = knHealthLeft }) (mn { fighterHealthPts = mnHealthLeft }) else -1
    --     else (fighterGold kn) + (fighterGold mn)
           
{- |
=🛡= Sum types

Another powerful ambassador of ADTs is the __sum type__. Unlike ordinary records
(product types) that always have all the fields you wrote, sum types represent
alternatives of choices. Sum types can be seen as "one-of" data structures. They
contain many product types (described in the previous section) as alternatives.

To define a sum type, you have to specify all possible constructors separated
by "|". Each constructor on its own could have an ADT, that describes
this branch of the alternative.

There is at least one famous sum type that you have already seen — 'Bool' — the
simplest example of a sum type.

@
data Bool = False | True
@

'Bool' is a representer of so-called __enumeration__ — a special case of sum
types, a sum of nullary constructors (constructors without fields).
Sum types can have much more than two constructors (but don't abuse this)!

Look at this one. We need more than two constructors to mirror the "Magic Type".
And none of the magic streams needs any fields. Just pure magic ;)

@
data MagicType
    = DarkMagic
    | LightMagic
    | NeutralMagic
@

However, the real power of sum types unleashes when you combine them with
fields. As we mentioned, each "|" case in the sum type could be an ADT, so,
naturally, you can have constructors with fields, which are product types from
the previous section. If you think about it, the enumeration also contains a
product type, as it is absolutely legal to create a data type with one
constructor and without any fields: `data Emptiness = TotalVoid`.

To showcase such sum type, let's represent a possible loot from successfully
completing an adventure:

@
data Loot
    = Sword Int  -- attack
    | Shield Int  -- defence
    | WizardStaff Power SpellLevel
@

You can create values of the sum types by using different constructors:

@
woodenSword :: Loot
woodenSword = Sword 2

adamantiumShield :: Loot
adamantiumShield = Shield 3000
@

And you can pattern match on different constructors as well.

@
acceptLoot :: Loot -> String
acceptLoot loot = case loot of
    Sword _ -> "Thanks! That's a great sword!"
    Shield _ -> "I'll accept this shield as a reward!"
    WizardStaff _ _ -> "What?! I'm not a wizard, take it back!"
@


To sum up all the above, a data type in Haskell can have zero or more
constructors, and each constructor can have zero or more fields. This altogether
gives us product types (records with fields) and sum types (alternatives). The
concept of product types and sum types is called __Algebraic Data Type__. They
allow you to model your domain precisely, make illegal states unrepresentable
and provide more flexibility when working with data types.
-}

{- |
=⚔️= Task 3

Create a simple enumeration for the meal types (e.g. breakfast). The one who
comes up with the most number of names wins the challenge. Use your creativity!
-}

data Meal
    = Breakfast String
    | Brunch String
    | Snack String
    | Lunch String
    | TeaTime String
    | Supper String
    | Appetizer String
    | MidnightSnack String
    | Feast String

{- |
=⚔️= Task 4

Define types to represent a magical city in the world! A typical city has:

⍟ Optional castle with a __name__ (as 'String')
⍟ Wall, but only if the city has a castle
⍟ Church or library but not both
⍟ Any number of houses. Each house has one, two, three or four __people__ inside.

After defining the city, implement the following functions:

 ✦ buildCastle — build a castle in the city. If the city already has a castle,
   the old castle is destroyed, and the new castle with the __new name__ is built
 ✦ buildHouse — add a new living house
 ✦ buildWalls — build walls in the city. But since building walls is a
   complicated task, walls can be built only if the city has a castle
   and at least 10 living __people__ inside in all houses of the city in total.
-}

data Person 
    = Person 
    deriving (Show)

data House = House 
    { houseResidents :: [Person]
    } deriving (Show)

data CityBuilding 
    = Church 
    | Library 
    deriving (Show)

data Castle = Castle
    { castleName    :: String,
      cityWalls     :: Int 
    } deriving (Show)


data MagicalCity = MagicalCity
    { cityBuilding    :: CityBuilding,
      houses          :: [House],
      cityCastle      :: Maybe Castle
    } deriving (Show)

isChurch :: CityBuilding -> Bool
isChurch (Church) = True
isChurch _ = False 

hasChurch :: MagicalCity -> Bool
hasChurch mC = isChurch (cityBuilding mC)

hasCastle :: MagicalCity -> Bool
hasCastle mC = not (isNothing (cityCastle mC))

wallCount :: Maybe Castle -> Int
wallCount c
    | not (isNothing c) = cityWalls (fromJust c)
    | otherwise = 0

cityWallsCount :: MagicalCity -> Int
cityWallsCount mC = wallCount (cityCastle mC)

cityResidentsCount :: MagicalCity -> Int
cityResidentsCount mC = sum (map (\x -> length (houseResidents x)) (houses mC))

testCity :: MagicalCity
testCity = MagicalCity 
    { cityBuilding = Church, 
    houses = [ House { houseResidents = [Person, Person] }, House { houseResidents = [Person, Person] } ],
    cityCastle = Just (Castle {castleName = "Magical Castle", cityWalls = 5}) 
    }
-- testCity = MagicalCity { cityBuilding = Church, houses = [ House { houseResidents = [Person, Person, Person, Person, Person, Person] }, House { houseResidents = [Person, Person, Person, Person, Person, Person] } ], cityCastle = Just (Castle {castleName = "Magical Castle", cityWalls = 5}) }
-- testCity = MagicalCity { cityBuilding = Church, houses = [House [Person]], cityCastle = Nothing }

buildCastle :: MagicalCity -> String -> MagicalCity
buildCastle mC castleN = mC { cityCastle = newCastle mC castleN}
    where
      newCastle :: MagicalCity -> String -> Maybe Castle
      newCastle mC castleN = if hasCastle mC then Just ((fromJust (cityCastle mC)) { castleName = castleN}) else Just (Castle {castleName = castleN, cityWalls = 0}) 

buildHouse :: MagicalCity -> MagicalCity
buildHouse mC =
    let peopleNum = if hasCastle mC then if hasChurch mC then 4 else 3 else if hasChurch mC then 2 else 1
    in mC {houses = House (replicate peopleNum Person) : houses mC}

buildWalls :: MagicalCity -> MagicalCity
buildWalls mC 
    | not (hasCastle mC) = mC
    | cityResidentsCount mC < 10 = mC
    | otherwise = mC { cityCastle = Just (newCastle mC) }
    where 
      newCastle :: MagicalCity -> Castle
      newCastle mC = (fromJust (cityCastle mC)) {cityWalls = (cityWallsCount mC) + 1} 

{-
=🛡= Newtypes

There is one more way to create a custom structure in Haskell. Let's see what
that is and how it differs from others.

__Newtype__ is a way to create a lightweight wrapper around an existing type.
Unlike type aliases, newtypes make an entirely new type from the compiler's point
of view (as the name suggests). However, such data types don't have additional
runtime overhead, which means that it would work as fast as the underlying type
without the wrapper.

You can declare a data type as a newtype only if it has __exactly one
constructor__ with __exactly one field__. It is a compiler error if you try to
define a newtype with another number of constructors or fields.

The syntax is similar to defining an ordinary data type, but you use the
"newtype" keyword instead of the "data" keyword. "newtype" is a product type.

@
newtype Attack = MkAttack Int
@

The same rule about names fields as for any data types applies to newtypes
as well. Meaning you can write the above type as follows:

@
newtype Attack = MkAttack
    { unAttack :: Int
    }
@

You can use the "MkAttack" constructor to create values of the "Attack" type,
and you can pattern match on "MkAttack" as on ordinary data types. When using
newtypes, you pay an extra development cost of writing extra wrappers and
unwrappers, but at the same time, you get additional compile-time guarantees of
not mixing types.

Newtypes serve the purpose of creating safer and more maintainable interfaces.
Let's prove that.

Say, we have a function to get a BMI.

@
myBMI :: Double -> Double -> Double
myBMI height weight = ...
@

And I can use it to calculate my BMI:

ghci> myBMI 200 70

Imagine how terrifying it could be if one accidentally messes with the order of
height and weight. 😱

ghci> myBMI 70 200

However, this could be avoided if our function would look like this:

@
myBMI :: Height -> Weight -> Double
--       |         ╰╴ newtype
--       ╰╴newtype
myBMI height weight = ...
@

And to run it you won't be able to mess arguments:

ghci> myBMI (Height 200) (Weight 70)
-}

{-
=⚔️= Task 5

Improve the following code (types definition and function implementations) by
introducing extra newtypes.

🕯 HINT: if you complete this task properly, you don't need to change the
    implementation of the "hitPlayer" function at all!
-}
newtype Attack = Attack
    { attackPts :: Int
    } deriving (Show)

newtype Health = Health
    { healthPts :: Int
    } deriving (Show)

newtype Armor = Armor
    { armorPts :: Int
    } deriving (Show)

newtype Dexterity = Dexterity
    { dexterityPts :: Int
    } deriving (Show)

newtype Strength = Strength
    { strengthPts :: Int
    } deriving (Show)

newtype Damage = Damage
    { damagePts :: Int
    } deriving (Show)

newtype Defense = Defense
  { defensePts :: Int
  } deriving (Show)   

data Player = Player
    { playerHealth    :: Health
    , playerArmor     :: Armor
    , playerAttack    :: Attack
    , playerDexterity :: Dexterity
    , playerStrength  :: Strength
    } deriving (Show)

calculatePlayerDamage :: Attack -> Strength -> Damage
calculatePlayerDamage attack strength = Damage ((attackPts attack) + (strengthPts strength))

calculatePlayerDefense :: Armor -> Dexterity -> Defense
calculatePlayerDefense armor dexterity = Defense ((armorPts armor) * (dexterityPts dexterity))

calculatePlayerHit :: Damage -> Defense -> Health -> Health
calculatePlayerHit damage defense health = Health ((healthPts health) + (defensePts defense) - (damagePts damage))

-- The second player hits first player and the new first player is returned
hitPlayer :: Player -> Player -> Player
hitPlayer player1 player2 =
    let damage = calculatePlayerDamage
            (playerAttack player2)
            (playerStrength player2)
        defense = calculatePlayerDefense
            (playerArmor player1)
            (playerDexterity player1)
        newHealth = calculatePlayerHit
            damage
            defense
            (playerHealth player1)
    in player1 { playerHealth = newHealth }

-- playa1 :: Player
-- playa1 = Player
--     { playerHealth = Health 10
--     , playerArmor = Armor 2
--     , playerAttack = Attack 3
--     , playerDexterity = Dexterity 4
--     , playerStrength = Strength 5
--     }

-- playa2 :: Player
-- playa2 = Player
--     { playerHealth = Health 20
--     , playerArmor = Armor 2
--     , playerAttack = Attack 1
--     , playerDexterity = Dexterity 4
--     , playerStrength = Strength 5
--     }
{- |

=🛡= Polymorphic data types

Similar to functions, data types in Haskell can be __polymorphic__. This means
that they can use some type variables as placeholders, representing general
types. You can either reason about data types in terms of such variables (and
don't worry about the specific types), or substitute variables with some
particular types.
Such polymorphism in Haskell is an example of __parametric polymorphism__.

The process of defining a polymorphic type is akin to an ordinary data type
definition. The only difference is that all the type variables should go after
the type name so that you can reuse them in the constructor fields later.

For example,

@
data Foo a = MkFoo a
@

Note that both product and sum types can be parameterised.

> Actually, we've already seen a polymorphic data type! Remember Lists from Chapter Two?

To give an example of a custom polymorphic type, let's implement a
"TreasureChest" data type. Our treasure chest is flexible, and it can store some
amount of gold. Additionally there is some space for one more arbitrary
treasure. But that could be any treasure, and we don't know what it is
beforehand.

In Haskell words, the data type can be defined like this:

@
data TreasureChest x = TreasureChest
    { treasureChestGold :: Int
    , treasureChestLoot :: x
    }
@

You can see that a treasure chest can store any treasure, indeed! We call it
treasure 'x'.

And when writing functions involving the "TreasureChest" type, we don't always
need to know what kind of treasure is inside besides gold.
We can either use a type variable in our type signature:

@
howMuchGoldIsInMyChest :: TreasureChest x -> Int
@

or we can specify a concrete type:

@
isEnoughDiamonds :: TreasureChest Diamond -> Bool
@

In the same spirit, we can implement a function that creates a treasure chest with some
predefined amount of gold and a given treasure:

@
mkMehChest :: x -> TreasureChest x
mkMehChest treasure = TreasureChest
    { treasureChestGold = 50
    , treasureChestLoot = treasure
    }
@


Polymorphic Algebraic Data Types are a great deal! One of the most common and
useful standard polymorphic types is __"Maybe"__. It represents the notion of
optional value (maybe the value is there, or maybe it is not).
"Maybe" is defined in the standard library in the following way:

@
data Maybe a
    = Nothing
    | Just a
@

Haskell doesn't have a concept of "null" values. If you want to work with
potentially absent values, use the "Maybe" type explicitly.

> Is there a good way to avoid null-pointer bugs? Maybe. © Jasper Van der Jeugt

Another standard polymorphic data type is "Either". It stores either the value
of one type or a value of another.

@
data Either a b
    = Left a
    | Right b
@

♫ NOTE: It can help to explore types of constructors "Nothing", "Just", "Left"
  and "Right". Let's stretch our fingers and blow off the dust from our GHCi and
  check that!

You can pattern match on values of the "Either" type as well as on any other
custom data type.

@
showEither :: Either String Int -> String
showEither (Left msg) = "Left with string: " ++ msg
showEither (Right n) = "Right with number: " ++ show n
@

Now, after we covered polymorphic types, you are finally ready to learn how
lists are actually defined in the Haskell world. Behold the mighty list type!

@
data [] a
    = []
    | a : [a]
@

Immediately we know what all of that means!
The ":" is simply the constructor name for the list. Constructors in Haskell can
be defined as infix operators as well (i.e. be written after the first argument,
the same way we write `1 + 2` and not `+ 1 2`), but only if they start with a
colon ":". The ":" is taken by lists. Now you see why we were able to pattern
match on it?

The type name uses built-in syntax to reserve the square brackets [] exclusively
for lists but, otherwise, is a simple polymorphic recursive sum type.

If you rename some constructor and type names, the list type could look quite
simple, as any of us could have written it:

@
data List a
    = Empty
    | Cons a (List a)
@

♫ NOTE: We use () to group "List" with "a" type variable in the second field of
  the "Cons" constructor. This is done to tell the compiler that "List" and "a"
  should go together as one type.
-}

{- |
=⚔️= Task 6

Before entering the real world of adventures and glorious victories, we should
prepare for different things in this world. It is always a good idea to
understand the whole context before going on a quest. And, before fighting a
dragon, it makes sense to prepare for different unexpected things. So let's
define data types describing a Dragon Lair!

 ⍟ A lair has a dragon and possibly a treasure chest (as described in the
   previous section). A lair also may not contain any treasures, but we'll never
   know until we explore the cave!
 ⍟ A dragon can have a unique magical power. But it can be literally anything!
   And we don't know in advance what power it has.

Create data types that describe such Dragon Lair! Use polymorphism to
parametrise data types in places where values can be of any general type.

🕯 HINT: 'Maybe' that some standard types we mentioned above are useful for
  maybe-treasure ;)
-}
data TreasureChest = TreasureChest String
    deriving (Show)

data Power a = Power a
    deriving (Show)

data Dragon x = Dragon
    { dragonPower :: Power x
    } deriving (Show)

data DragonLair dPower = DragonLair
    { dragon :: Dragon dPower,
      treasureChest :: Maybe TreasureChest
    } deriving (Show)

{-
=🛡= Typeclasses

__Typeclass__ is a regularly used way to express common characteristics of
different data types. In some sense, a typeclass describes the interface of some
value without telling you the implementation details.

__Instance__ is a representation of the typeclass ↔︎️ data type relationships. In
order to show that the data type obeys the typeclasses rules and to use the
methods of the typeclass on the data values, you need to provide the work
instructions under this particular typeclass. And that is the instance of the
data type for the specific typeclass.

Let’s consolidate the typeclasses and instances concepts on the analogues from
our fantasy world.

Many lovely princesses need to be rescued. And those processes are usually
alike: find a path to the particular castle (differs from princess to princess),
defeat the monster (also unique for each princess) and rescue the beloved
beauty. So we can see that the "Rescue Mission Plan" is the typeclass, and each
princess has its own instance for that. If you are a prince on a white horse,
you'd better check the particular instance for your princess to get on the
salvation journey.

Next, let’s look at one code example for a better illustration of the
instance-typeclass relationship. We can define a typeclass that would tell us
one's arch enemy.

The syntax is as follows: you need to use the "class" keyword, then you need to
specify the typeclass name. Typeclasses should start with an upper case letter.
After that, the type parameter should be identified, which represents the data
types that would have instances of this typeclass. And, finally, the "where"
keyword. After, you can specify methods of the typeclass – functions that should
work with the type parameter.

@
      ┌─ typeclass name
      │
      │         ┌─ type parameter
      │         │   ┌───── "where" keyword
      │         │   │
class ArchEnemy a where
 │  getArchEnemy :: a -> String
 │  │               │
 │  │               └─── the same type parameter
 │  └───── method name
 │
 └ "class" keyword
@

And that 'getArchEnemy' method could be used with a lot of data types: Bool,
Double,… name them all! Let’s have our first instances to show how it works:

The syntax is simple and consistent with the typeclass declaration, but instead
of the "class" keyword you need to have the "instance" keyword. Of course,
instead of the type parameter, you have to specify the concrete type, for which
you are implementing the instance. And all the necessary methods should have its
implementation for the particular data type.

@
instance ArchEnemy Bool where
    getArchEnemy :: Bool -> String
    getArchEnemy True = "False"
    getArchEnemy False = "True"


instance ArchEnemy Int where
    getArchEnemy :: Int -> String
    getArchEnemy i = case i of
        0 -> "Division"
        _ -> "Derivative"

instance ArchEnemy Double where
    getArchEnemy :: Double -> String
    getArchEnemy n
        | isNaN n = "Infinity"
        | isInfinite n = "Limit"
        | otherwise = "NaN"
@

And then you can write polymorphic functions and not worry about which specific
type is under the hood until it has the instance of the desired typeclass. For that
we are using __constraints__ in Haskell. It is the identification of affiliation
to the typeclass. The constraints should go after the "::" sign in the function
type declaration. You can specify one or many constraints. If more than one they
should be in parentheses and comma-separated. The end of constraints is
determined with the "=>" arrow, and the function type could be written as usual.

@
revealArchEnemy :: (ArchEnemy a, Show a) => a -> String
revealArchEnemy x =
    "The arch-enemy of " ++ show x ++ " is " ++ getArchEnemy x
@

The behaviour of this polymorphic function depends on the data type used with
this function. Such dependency is called __ad-hoc polymorphism__.

This is how it works in action:

ghci> revealArchEnemy (42 :: Int)
"The arch-enemy of 42 is Derivative"

However, if we try to use this function with something that doesn’t implement an
instance of our typeclass, we will get the corresponding compiler error, that
would warn us precisely about that:

ghci> revealArchEnemy "An adorable string that has no enemies (✿◠ω◠)"

<interactive>:21:1: error:
    • No instance for (ArchEnemy String)
        arising from a use of revealArchEnemy
    • In the expression: revealArchEnemy "An adorable string that has no enemies (✿◠ω◠)"
      In an equation for 'it': it = revealArchEnemy "An adorable string that has no enemies (✿◠ω◠)"


Interestingly, it is possible to reuse existing instances of data types in the
same typeclass instances as well. And we also can reuse the __constraints__ in
the instance declaration for that!

This gives us the ability to specify the instances for polymorphic data types
with some conditions (constraints).

Let's see it in the example of the 'ArchEnemy' typeclass instance for the
"Maybe" something data type.

@
instance (ArchEnemy a) => ArchEnemy (Maybe a) where
    getArchEnemy :: Maybe a -> String
    getArchEnemy (Just x) = getArchEnemy x
    getArchEnemy Nothing = "NullPointerException"
@

This instance is suitable for any Maybe as long as the instance of the inside
type exists. You can see how we reuse the fact that the underlying type has this
instance and apply this typeclass method to it.
-}

{- |
=⚔️= Task 7

Often we want to combine several values of a type and get a single value of the
exact same type. We can combine different things: treasures, adventures, groups
of heroes, etc.. So it makes sense to implement a typeclass for such a concept
and define helpful instances.

We will call such a typeclass "Append". You can find its definition below.

Implement instances of "Append" for the following types:

  ✧ The "Gold" newtype where append is the addition
  ✧ "List" where append is list concatenation
  ✧ *(Challenge): "Maybe" where append is appending of values inside "Just" constructors

-}
data Gold = Gold
  { gold  :: Int
  } deriving (Show)

class Append a where
    append :: a -> a -> a

instance Append Gold where
    append :: Gold -> Gold -> Gold
    append x y = Gold((gold x) + (gold y))

instance Append [a] where
    append :: [a] -> [a] -> [a]
    append x y = concat [x, y]

instance (Append a) => Append (Maybe a) where
    append :: Maybe a -> Maybe a -> Maybe a
    append (Just x) (Just y) = Just (append x y)
    append Nothing (Just y) = Just y
    append (Just x) Nothing = Just x
    append Nothing Nothing = Nothing

{-
=🛡= Standard Typeclasses and Deriving

As well as many useful data types, the standard library in Haskell also comes with some very
essential typeclasses:

 ﹡ 'Show' — show the data type value as a 'String'. This helps us to print to
   the terminal in GHCi.
 ﹡ 'Read' — inverse to the 'Show'. By a given String, it is possible to parse
   it into the data type.
 ﹡ 'Eq' — determine if two values of the same data types are equal.
 ﹡ 'Ord' — compare values of the same data type (requires the data type to have
   the instance of 'Eq').
 ﹡ 'Bounded' — specify the lowest and highest value of the data type
 ﹡ 'Enum' — operate with enumeration types.

You can use Hoogle to check what methods these classes have. Additionally, the
documentation there shows for which types there are already instances of these
typeclasses.

Alternatively, you can use the ":i" command in GHCi (short for ":info") to see
the typeclass methods and its instances for the standard data types.

As these typeclasses are way too useful, and it is relatively straightforward to
implement instances for such classes, GHC provides a nice feature: __deriving__.
Deriving is the feature of the Haskell compiler, that auto generates the
instances of typeclasses for the specified data types. The code would be
produced internally during the compilation, so no need to litter your code with
the boilerplate implementation of instances.

The deriving syntax is the following. It should be attached to the data type
declaration. It consists of the "deriving" keyword, and then typeclass names in
parentheses comma-separated.

@
data Princess = Princess
    { princessName :: String
    } deriving (Show, Read, Eq, Ord)
@

And it is ready to be used!

ghci> show (Princess "Anna")
Princess {princessName = "Anna"}

-}

{-
=⚔️= Task 8

Create an enumeration type to represent days of the week. After defining a type,
implement the following functions:

 ✦ isWeekend — True if the weekday is weekend
 ✦ nextDay — returns the next weekday
 ✦ daysToParty — number of the days to the next Friday

🕯 HINT: to implement this task, derive some standard typeclasses
-}
data DayOfWeek
    = Monday 
    | Tuesday 
    | Wednesday 
    | Thursday 
    | Friday
    | Saturday
    | Sunday
    deriving(Show, Enum, Bounded)

isWeekend :: DayOfWeek -> Bool
isWeekend (Saturday) = True
isWeekend (Sunday) = True
isWeekend (_) = False

nextDay :: DayOfWeek -> DayOfWeek
nextDay (Sunday) = Monday
nextDay d = succ d

daysToParty :: DayOfWeek -> Int
daysToParty (Friday) = 0
daysToParty d 
    | (fromEnum d) < (fromEnum Friday) = (fromEnum Friday) - (fromEnum d)
    | otherwise = (abs ((fromEnum Friday) - (fromEnum d))) + (fromEnum Friday)
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

newtype HealthPoints = HealthPoints
    { healthPoints    :: Int
    } deriving (Show)

newtype DefencePoints = DefencePoints
    { defencePoints   :: Int
    } deriving (Show)    

data KnightAttack
    = KnightAttacking 
    | Heal 
    | IncreaseDefence 
    deriving(Show, Enum, Bounded, Eq, Ord)

data MonsterAttack
    = MonsterAttacking
    | RunAway 
    deriving(Show, Enum, Bounded, Eq, Ord)

data Knight = Knight 
    { knightHealth           :: HealthPoints,
      knightDefence          :: DefencePoints,
      knightsAttackPower     :: Int,
      knightsLastAttack      :: Maybe KnightAttack
    } deriving(Show)

testKnight :: Knight
testKnight = Knight { knightHealth = HealthPoints { healthPoints = 9 }, knightDefence = DefencePoints { defencePoints = 3}, knightsAttackPower = 5, knightsLastAttack = Nothing}

data Monster = Monster
    { monsterHealth           :: HealthPoints,
      monstersAttackPower     :: Int,
      monstersLastAttack      :: Maybe MonsterAttack
    } deriving(Show)

testMonster :: Monster
testMonster = Monster { monsterHealth = HealthPoints { healthPoints = 24 }, monstersAttackPower = 1, monstersLastAttack = Nothing}

testMonster2 :: Monster
testMonster2 = Monster { monsterHealth = HealthPoints { healthPoints = 15 }, monstersAttackPower = 13, monstersLastAttack = Nothing}

data Warrior
    = WarMonster { warMonster :: Monster }
    | WarKnight  { warKnight :: Knight }
    deriving(Show)

class DoAttack a b where
    doAttack :: a -> b -> (a, b)
    
instance DoAttack Knight Knight where
    doAttack :: Knight -> Knight -> (Knight, Knight)
    doAttack aK dK 
        | isNothing (knightsLastAttack aK) || (knightsLastAttack aK) == Just IncreaseDefence 
            = (aK { knightsLastAttack = Just KnightAttacking }, dK { knightHealth = HealthPoints { healthPoints = let curAtPts = defencePoints (knightDefence dK) - (knightsAttackPower aK) in if curAtPts > 0 then (getHealthNum dK) + curAtPts else getHealthNum dK}})
        | (knightsLastAttack aK) == Just KnightAttacking 
            = (aK { knightHealth = HealthPoints { healthPoints = (healthPoints (knightHealth aK)) + 1}, knightsLastAttack = Just Heal}, dK)
        | otherwise = (aK { knightDefence = DefencePoints { defencePoints = (defencePoints (knightDefence aK)) + 1}, knightsLastAttack = Just IncreaseDefence}, dK)

instance DoAttack Knight Monster where
    doAttack :: Knight -> Monster -> (Knight, Monster)
    doAttack aK dM 
        | isNothing (knightsLastAttack aK) || (knightsLastAttack aK) == Just IncreaseDefence 
            = (aK { knightsLastAttack = Just KnightAttacking }, dM { monsterHealth = HealthPoints { healthPoints = healthPoints (monsterHealth dM) - (knightsAttackPower aK)}})
        | (knightsLastAttack aK) == Just KnightAttacking 
            = (aK { knightHealth = HealthPoints { healthPoints = (healthPoints (knightHealth aK)) + 1}, knightsLastAttack = Just Heal}, dM)
        | otherwise = (aK { knightDefence = DefencePoints { defencePoints = (defencePoints (knightDefence aK)) + 1}, knightsLastAttack = Just IncreaseDefence}, dM)

instance DoAttack Monster Monster where
    doAttack :: Monster -> Monster -> (Monster, Monster)
    doAttack aM dM 
        | isNothing (monstersLastAttack aM) || (monstersLastAttack aM) == Just RunAway 
            = (aM { monstersLastAttack = Just MonsterAttacking }, dM { monsterHealth = HealthPoints { healthPoints = healthPoints (monsterHealth dM) - (monstersAttackPower aM)}})
        | otherwise = (aM { monstersLastAttack = Just RunAway }, dM)

instance DoAttack Monster Knight where
    doAttack :: Monster -> Knight -> (Monster, Knight)
    doAttack aM dK 
        | isNothing (monstersLastAttack aM) || (monstersLastAttack aM) == Just RunAway 
            = (aM { monstersLastAttack = Just MonsterAttacking }, dK { knightHealth = HealthPoints { healthPoints = let curAtPts = defencePoints (knightDefence dK) - (monstersAttackPower aM) in if curAtPts > 0 then (getHealthNum dK) + curAtPts else getHealthNum dK}})
        | otherwise = (aM { monstersLastAttack = Just RunAway }, dK)

class GetHealthNum a where
    getHealthNum :: a -> Int

instance GetHealthNum Knight where
    getHealthNum :: Knight -> Int
    getHealthNum k = healthPoints (knightHealth k) 

instance GetHealthNum Monster where
    getHealthNum :: Monster -> Int
    getHealthNum m = healthPoints (monsterHealth m) 

class IsDead a where 
    isDead :: a -> Bool

instance IsDead Knight where
    isDead :: Knight -> Bool
    isDead k = if (getHealthNum k) > 0 then False else True

instance IsDead Monster where
    isDead :: Monster -> Bool
    isDead m = if (getHealthNum m) > 0 then False else True

data BattleOutcome = BattleOutcome
    { winner       :: Maybe Warrior,
      totalRounds  :: Int
    } deriving(Show)

finalFight :: Warrior -> Warrior -> BattleOutcome
finalFight w1 w2 = go 1 w1 w2
    where 
      go :: Int -> Warrior -> Warrior -> BattleOutcome
      go n (WarMonster w1) (WarMonster w2) 
          | monstersAttackPower w1 < 1 && monstersAttackPower w2 < 1 = BattleOutcome { winner = Nothing, totalRounds = n}
          | isDead w1 = BattleOutcome { winner = Just (WarMonster { warMonster = w2 }), totalRounds = n}
          | otherwise = let attackResult = doAttack w1 w2
              in if isDead (snd attackResult) then BattleOutcome { winner = Just (WarMonster { warMonster = w1 }), totalRounds = n} else 
                  let sndAttackResult = doAttack (snd attackResult) (fst attackResult) in go (n+1) WarMonster { warMonster = (snd sndAttackResult)} WarMonster { warMonster = (fst sndAttackResult)}
      go n (WarMonster w1) (WarKnight w2) 
          | isDead w1 = BattleOutcome { winner = Just (WarKnight { warKnight = w2 }), totalRounds = n}
          | (defencePoints (knightDefence w2) - (monstersAttackPower w1)) > 0 && knightsAttackPower w2 < 1 = BattleOutcome { winner = Nothing, totalRounds = n}
          | otherwise = let attackResult = doAttack w1 w2
              in if isDead (snd attackResult) then BattleOutcome { winner = Just (WarMonster { warMonster = w1 }), totalRounds = n} else 
                  let sndAttackResult = doAttack (snd attackResult) (fst attackResult) in go (n+1) WarMonster { warMonster = (snd sndAttackResult)} WarKnight { warKnight = (fst sndAttackResult)}
      go n (WarKnight w1) (WarMonster w2)
          | isDead w1 = BattleOutcome { winner = Just (WarMonster { warMonster = w2 }), totalRounds = n}
          | (defencePoints (knightDefence w1) - (monstersAttackPower w2)) > 0 && knightsAttackPower w1 < 1 = BattleOutcome { winner = Nothing, totalRounds = n}
          | otherwise = let attackResult = doAttack w1 w2
              in if isDead (snd attackResult) then BattleOutcome { winner = Just (WarKnight { warKnight = w1 }), totalRounds = n} else 
                  let sndAttackResult = doAttack (snd attackResult) (fst attackResult) in go (n+1) WarKnight { warKnight = (snd sndAttackResult)} WarMonster { warMonster = (fst sndAttackResult)}
      go n (WarKnight w1) (WarKnight w2)
          | isDead w1 = BattleOutcome { winner = Just (WarKnight { warKnight = w2 }), totalRounds = n}
          | (defencePoints (knightDefence w1) - (knightsAttackPower w2)) > 0 && (defencePoints (knightDefence w2) - (knightsAttackPower w1)) > 0 = BattleOutcome { winner = Nothing, totalRounds = n}
          | otherwise = let attackResult = doAttack w1 w2
              in if isDead (snd attackResult) then BattleOutcome { winner = Just (WarKnight { warKnight = w1 }), totalRounds = n} else 
                  let sndAttackResult = doAttack (snd attackResult) (fst attackResult) in go (n+1) WarKnight { warKnight = (snd sndAttackResult)} WarKnight { warKnight = (fst sndAttackResult)}


{-
You did it! Now it is time to open pull request with your changes
and summon @vrom911 and @chshersh for the review!
-}

{-
=📜= Additional resources
Deriving: https://kowainik.github.io/posts/deriving
Extensions: https://kowainik.github.io/posts/extensions
-}

{-
In this game, the player (you) and the enemy (the boss) take turns attacking. The player always goes first. Each attack reduces the opponent's hit points by at least 1. The first character at or below 0 hit points loses.

Damage dealt by an attacker each turn is equal to the attacker's damage score minus the defender's armor score. An attacker always does at least 1 damage. So, if the attacker has a damage score of 8, and the defender has an armor score of 3, the defender loses 5 hit points. If the defender had an armor score of 300, the defender would still lose 1 hit point.

Your damage score and armor score both start at zero. They can be increased by buying items in exchange for gold. You start with no items and have as much gold as you need. Your total damage or armor is equal to the sum of those stats from all of your items. You have 100 hit points.

You must buy exactly one weapon; no dual-wielding. Armor is optional, but you can't use more than one. You can buy 0-2 rings (at most one for each hand). You must use any items you buy. The shop only has one of each item, so you can't buy, for example, two rings of Damage +3.

For example, suppose you have 8 hit points, 5 damage, and 5 armor, and that the boss has 12 hit points, 7 damage, and 2 armor:

    The player deals 5-2 = 3 damage; the boss goes down to 9 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 6 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 6 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 4 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 3 hit points.
    The boss deals 7-5 = 2 damage; the player goes down to 2 hit points.
    The player deals 5-2 = 3 damage; the boss goes down to 0 hit points.

In this scenario, the player wins! (Barely.)

You have 100 hit points. The boss's actual stats are in your puzzle input. What is the least amount of gold you can spend and still win the fight?

--- Part Two ---

Turns out the shopkeeper is working with the boss, and can persuade you to buy whatever items he wants. The other rules still apply, and he still only has one of each item.

What is the most amount of gold you can spend and still lose the fight?

-}
data Item = Item
    { name   :: String
    , cost   :: Int
    , damage :: Int
    , armor  :: Int
    } deriving (Show, Eq)

data Player = Player 
    { pHP     :: Int
    , pDamage :: Int 
    , pArmor  :: Int
    , pCost   :: Int
    } deriving (Show, Eq)

weapons :: [Item]
weapons =  
    [Item {name = "Dagger"       , cost =  8, damage = 4, armor = 0},
     Item {name = "Shortsword"   , cost = 10, damage = 5, armor = 0},
     Item {name = "Warhammer"    , cost = 25, damage = 6, armor = 0},
     Item {name = "Longsword"    , cost = 40, damage = 7, armor = 0},
     Item {name = "Greataxe"     , cost = 74, damage = 8, armor = 0}]

armors :: [Item]
armors =     
    [Item {name = "No Armor"     , cost =  0, damage = 0, armor = 0},
     Item {name = "Leather"      , cost = 13, damage = 0, armor = 1},
     Item {name = "Chainmail"    , cost = 31, damage = 0, armor = 2},
     Item {name = "Splintmail"   , cost = 53, damage = 0, armor = 3},
     Item {name = "Bandedmail"   , cost = 75, damage = 0, armor = 4},
     Item {name = "Platemail"    , cost =102, damage = 0, armor = 5}]
     
rings :: [Item]
rings =
    [Item {name = "No Left Ring" , cost =  0, damage = 0, armor = 0},
     Item {name = "No Right Ring", cost =  0, damage = 0, armor = 0},
     Item {name = "Damage +1"    , cost = 25, damage = 1, armor = 0},
     Item {name = "Damage +2"    , cost = 50, damage = 2, armor = 0},
     Item {name = "Damage +3"    , cost =100, damage = 3, armor = 0},
     Item {name = "Defense +1"   , cost = 20, damage = 0, armor = 1},
     Item {name = "Defense +2"   , cost = 40, damage = 0, armor = 2},
     Item {name = "Defense +3"   , cost = 80, damage = 0, armor = 3}]

itemSets :: [Item]
itemSets = [Item {name   = name w ++ ", " ++ 
                           name a ++ ", " ++ 
                           name r1 ++ ", " ++ 
                           name r2, 
                  cost   = cost w + cost a + cost r1 + cost r2,
                  damage = damage w + damage a + damage r1 + damage r2,
                  armor  = armor w + armor a + armor r1 + armor r2}
            | w <- weapons, 
              a <- armors, 
              r1 <- rings, 
              r2 <- rings, 
              r1 /= r2]

players :: [Player]
players = [Player {pHP     = 100,
                   pDamage = damage i,
                   pArmor  = armor i,
                   pCost   = cost i}
                  | i <- itemSets ]

boss :: Player
boss = Player {pHP = 104, pDamage = 8, pArmor = 1, pCost = 0}

wins :: Player -> Bool
wins player = pHitsNeeded <= bHitsNeeded
    where
    pHit = max 1 (pDamage player - pArmor boss)
    bHit = max 1 (pDamage boss - pArmor player)
    pHitsNeeded = ceiling (fromIntegral (pHP boss) / fromIntegral pHit)
    bHitsNeeded = ceiling (fromIntegral (pHP player) / fromIntegral bHit)

minWinCost :: Int
minWinCost = (minimum . map snd . filter fst) $ 
            zip (map wins players) (map pCost players)

maxLoseCost :: Int
maxLoseCost = (maximum . map snd . filter (not . fst)) $ 
            zip (map wins players) (map pCost players)

main :: IO ()
main = do
    print "Answer Part 1:"
    print minWinCost

    print "------"
    print "Answer Part 2:"
    print maxLoseCost

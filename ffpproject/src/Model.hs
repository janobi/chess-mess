module Model where

-- GENERAL COMMENT: Yes, we're aware that oftentimes, our lens functions could be written shorter by hiding / only implying the input parameter (e.g. the Game g in getGameState). We deliberately opted not to shorten these functions for better and quicker readability.

import Control.Lens
import Data.List
import System.Random
import Data.Maybe
import Utils


import TypeDefs

-------

-- game setup constants
initPlayerHealth = 20 --initial player health
fieldRows = 12 --number of field rows
fieldColumns = 12 --number of field columns
startUnits = 3 -- number of initially spawned units
cardsPerTurn = 4 --number of cards that can be played per round
cardAmount = 11 --number of slots for cards per player

-- | Pool from which cards are drawn (exhaustive)
cardPool :: [CardID]
cardPool = [Card1, Card2, Card2, Card3, Card4, Card4, Card5, Card5, Card6, Card7, Card8, Card8, Card8, Card8, Card8, Card8, Card8, Card8, Card8, Card8, Card9, Card10, Card10]


deadUnit = Just Unit {
    _uStrength = -1,
    _uPlayer = PlayerB,
    _hasAttacked = False
}

-----------------------------------------------
---             INIT AND RESET              ---
-----------------------------------------------


-- initial game data

initPlayerCards = replicate cardAmount Card{
    _cPlayer = PlayerA,
    _cId = Card_None,
    _cName = "",
    _cFnct = id,
    _cVars = ([],[]),
    _cInfoOffset = Nothing,
    _listPosition = 0
}

initRow :: [Maybe a]
initRow = replicate fieldColumns Nothing

initField = replicate fieldColumns  $ replicate fieldRows Nothing --2D list of $fieldColumns x $fieldRows empty Unit slots

initPlayerA = Player {
    _pId = PlayerA,
    _pHealth = initPlayerHealth,
    _pCards = initPlayerCards,
    _pBench = initRow,
    _pActiveCard = head initPlayerCards
}
initPlayerB = Player {
    _pId = PlayerB,
    _pHealth = initPlayerHealth,
    _pCards = initPlayerCards,
    _pBench = initRow,
    _pActiveCard = head initPlayerCards
}
initGame = Game {
    _state = GS_NotStarted,
    _activePlayer = PlayerA,
    _players = [initPlayerA, initPlayerB],
    _field = initField,
    _rounds = 0,
    _waitingOpID = "",
    _playableCardsLeft = 4
}

-- | removes all Units on the field 
resetField :: Game -> Game
resetField g = set field initField g

-- | Resets health of both players to the starting const in turn of starting a new round
resetPHealth :: Game -> Game
resetPHealth g = set players ( map (set pHealth initPlayerHealth ) (view players g)) g

-- | return the given 'Game' with the 'state' component set to 'GS_WaitingForPlayer'
startGame :: Game -> Game
startGame g = set state GS_WaitingForPlayer g

-- | reset the given 'Game'
resetGame :: Game -> Game
resetGame _ = initGame

-- | reset the 'waitingOpID' component of the given Game
resetWaitingOpID :: Game -> Game
resetWaitingOpID g = set waitingOpID "" g

-- | Init player bench at start of each Turn with x random low level untis
-- | pId -> ramdSeed -> game -> game
initPBench :: PlayerID -> Int ->   Game -> Game
initPBench pId   randSeed g    = do
    let range = 0.99 :: Float
    let randList = randomRs(0, range)(mkStdGen randSeed )
    let units = take fieldRows (convertToUnits pId (take startUnits randList) ++ (replicate fieldRows Nothing))
    setPBench pId units g

-- | give out new cards randomly, drawn from an exhaustive card pool
giveOutCards :: Int -> PlayerID -> Game -> Game
giveOutCards randSeed pId g = do
    let range = (length cardPool) - 1
    let randomNumber = randomRs(0, range) (mkStdGen randSeed)

    let randomSeeds = randomRs(minBound :: Int, maxBound :: Int) (mkStdGen randSeed)


    let cards = generateRandomDeck cardAmount pId randomSeeds cardPool

    setPCards pId cards g


-- | Generates randomly varied cards according to given list with CardIDs
--     amount ->    pId ->    CardPool (to limit number of cards which can appear of each sort) -> set of rand seeds
generateRandomDeck ::Int -> PlayerID -> [Int] -> [CardID] -> [Card]
generateRandomDeck 0  pId (r:rands) pool = []
generateRandomDeck am pId (r:rands) pool = do
    let rIndex = fst(randomR(0, (length pool) -1) (mkStdGen r))
    [generateCard r pId (pool!!rIndex)] ++ generateRandomDeck (am -1) pId rands (remove rIndex pool)
generateRandomDeck _ _ _ _               = []

-- | Takes a playerId and a list of random values [0.00 .. 0.99 :: Float] and converts them to a list of low level fieldCells in context of initializing playerBench
convertToUnits :: PlayerID -> [Float] -> [FieldCell]
convertToUnits player [] = []
convertToUnits player (r:randlist) | r >  0.98 = [generateUnit player 3] ++ convertToUnits player randlist
                                   | r >= 0.94 = [generateUnit player 2] ++ convertToUnits player randlist
                                   | r >= 0.64 = [generateUnit player 1] ++ convertToUnits player randlist
                                   | r <  0.64 = [generateUnit player 0] ++ convertToUnits player randlist
convertToUnits player _  = []


----------------------------------------
---     GETTER SETTER AND OVER       ---
----------------------------------------


-- Game --
----------

-- | return the 'state' component of the given 'Game'
getGameState :: Game -> GameState
getGameState g = view state g

-- | return the current 'Field' for a given 'Game'
getField :: Game -> Field
getField g = view field g

-- | return whether or not one of the players has health <= 0 (the winner is the last-attacking player, i.e. Game.activePlayer)
isGameFinished :: Game -> Bool
isGameFinished g = any (<=0) playerHealths where
    playerHealths = map (view pHealth) (view players g)

-- | return the 'activePlayer' component of the given 'Game'
getActivePlayer :: Game -> PlayerID
getActivePlayer g = view activePlayer g

-- | return the 'roundCounter' component of the given 'Game'
getRoundCounter :: Game -> Int
getRoundCounter g = view rounds g

-- | return the 'waitingOpID' component of the given Game
getWaitingOpID :: Game -> String
getWaitingOpID g = view waitingOpID g

-- | return the 'Player' data for the given 'Game' and 'PlayerID'
getPlayer :: PlayerID -> Game -> Player
getPlayer pId_ g = head $ filter ((pId_ ==) . getPlayerId) (view players g)

-- | set the given 'GameState' in a given 'Game' and return the resulting 'Game'
setGameState :: GameState -> Game -> Game
setGameState gs g = set state gs g

-- | set the 'waitingOpID' component of the given Game
setWaitingOpID :: String -> Game -> Game
setWaitingOpID newOp g = set waitingOpID newOp g

-- | return the given 'Game' with the 'activePlayer' component toggled to the other player
switchToNextPlayer :: Game -> Game
switchToNextPlayer g = over activePlayer switch g

-- | return the given 'Game' with the 'rounds' component incremented
incrementRoundCounter :: Game -> Game
incrementRoundCounter g = over rounds (+1) g


-- Player --
------------

-- | set the 'pCards' component for a given 'PlayerID' and 'Game'. Testing. [String] instead of [Cards]
setPCards :: PlayerID -> [Card] -> Game -> Game
setPCards pId cards g = case pId of
    PlayerA -> do
        let playerA = getPlayer PlayerA g
        let playerB = getPlayer PlayerB g
        let playerA' = set pCards cards playerA
        set players [playerA', playerB] g

    PlayerB -> do
        let playerA = getPlayer PlayerA g
        let playerB = getPlayer PlayerB g
        let playerB'= set pCards cards playerB
        set players [playerA, playerB'] g


-- Player Getter

-- | return the 'PlayerID' of 'Player' 
getPlayerId :: Player -> PlayerID
getPlayerId p = view pId p

-- | return the 'pHealth' component for the player with the given 'PlayerID' in the given 'Game'
getPHealth :: PlayerID -> Game -> Int
getPHealth pId g = view pHealth $ getPlayer pId g

-- | return the 'pBench' component for a given 'PlayerID' and 'Game'
getPBench :: PlayerID -> Game -> FieldRow
getPBench pId g = view pBench $ getPlayer pId g

-- | return the 'pCards' component for the player with the given 'PlayerID' in the given 'Game'
getPCards :: PlayerID -> Game -> [Card]
getPCards pId g = view pCards $ getPlayer pId g

-- | return the currently viewed card by the active Player
getActivePCard :: PlayerID -> Game -> Card
getActivePCard pId g = view pActiveCard $ getPlayer pId g

-- | set the 'Player' with the given 'PlayerID' in the given 'Game' and return the resulting 'Game
setPlayer :: PlayerID -> Player -> Game -> Game
setPlayer playerId newPlayer g = over (players.traverse) (setRightPlayer) g where
    setRightPlayer p = if getPlayerId p == playerId then newPlayer else p

-- | set the 'pBench' component for a given 'PlayerID' and 'Game' and return the resulting 'Game'
setPBench :: PlayerID -> FieldRow -> Game -> Game
setPBench pId fr g = setPlayer pId (set pBench fr $ getPlayer pId g) g

-- | The card the player is currently seeing in the preview and which is played on button press
setActivePCard :: PlayerID -> Card -> Game -> Game
setActivePCard pId c g = setPlayer pId (set pActiveCard c $ getPlayer pId g) g

-- | apply a given modifier for the health of the given player
modifyPlayerHealth :: PlayerID -> (Int -> Int) -> Game -> Game
modifyPlayerHealth pId op g = setPlayer pId (over pHealth op $ getPlayer pId g) g



--  Card   --
-------------

-- | return the 'cId' component of the given 'Card'
getCardId :: Card -> CardID
getCardId = view cId

-- | return the 'listPosition' component of the given 'Card'
getCardPosition :: Card -> Int
getCardPosition = view listPosition

-- | return the first valid card position in the card list of the player with the given playerID
getFirstValidCardPositionInList :: Int -> PlayerID -> Game -> Int
getFirstValidCardPositionInList fromIndex playerID game = do
    let cardList = getPCards playerID game
    let cardID = getCardId (cardList !! fromIndex)
    if fromIndex >= length cardList then 0 else if cardID /= Card_None then fromIndex else getFirstValidCardPositionInList (fromIndex+1) playerID game

-- | return the 'cFnt' component of the given 'Card'
getCardFnct :: Card -> CardFunction
getCardFnct = view cFnct

-- | return the 'cPlayer' component of the given 'Card'
getCardPlayer :: Card -> PlayerID
getCardPlayer = view cPlayer

-- | return the unwrapped 'cInfoOffset' component of the given 'Card'
getCardInfoOffset :: Card -> ((Double,Double),(Double,Double))
getCardInfoOffset card = fromMaybe ((0,0),(0,0)) (view cInfoOffset card)

-- | return the 'cVars' component of the given 'Card'
getCardVariation :: Card -> ([Int],[Int])
getCardVariation c = view cVars c

-- | set the 'listPosition' component of the given 'Card' to the given number
setCardPosition :: Int -> Card -> Card
setCardPosition pos card = set listPosition pos card

-- | reset the 'playableCardsLeft' component of the given 'Game'
resetPlayableCards :: Game -> Game
resetPlayableCards g = set playableCardsLeft cardsPerTurn g

-- | return the 'playableCardsLeft' component of the given 'Game'
getPlayableCardsLeft :: Game -> Int
getPlayableCardsLeft g = view playableCardsLeft g


---------------------------------------
--         COMPLEX OPERATIONS        --
--------------------------------------- 

-- Execute Card --
------------------

-- | if playable cards are left, modify the player bench with the card's effect and decrement playable cards left
playCard ::  Card -> Game -> Game
playCard  c g = let p = getCardPlayer c in do
    if view playableCardsLeft g > 0 then
        do over playableCardsLeft ((+) (-1)) $ (setPlayer p (over pBench (getCardFnct c) $ getPlayer p g)) g
    else
        do g


-- Execute Field Logic --
-------------------------

-- | inits execution of events on the field corresponding to the currently active player
-- activePlayer -> game -> game
calculatePDamage :: PlayerID -> Game -> Game
calculatePDamage PlayerA g = do
    let fieldr = reverse (getField g) -- prevent healing through -1 units
    let playerDamage = foldr (+) 0 (map ((max 0) . getUnitStrength) (filter (\unit -> getUnitPlayer unit == PlayerA ) (catMaybes(head fieldr))))
    modifyPlayerHealth PlayerB ((+) (-playerDamage)) g
calculatePDamage PlayerB g = do
    let fieldr = getField g
    let playerDamage = foldr (+) 0 (map ((max 0) . getUnitStrength) (filter (\unit -> getUnitPlayer unit == PlayerB ) (catMaybes(head fieldr))))
    modifyPlayerHealth PlayerA ((+) (-playerDamage)) g

-- | updates the field with the status after moving all units. reverse direction corresponding to player. init field traversal
executeAttacks :: Game -> Game
executeAttacks g = do
    let field = getField g
    let activePlayer = getActivePlayer g
    let firstRowNew = getPBench activePlayer g

    if activePlayer == PlayerB
        then do

            --padding (empty row and player bench)
            let fieldrev   = reverse ([initRow] ++ field ++ [firstRowNew] )

            --reverse list for mirrored player to operate on it, reverse it again for updating game
            let newField = reverse(rowsAttack PlayerB (fieldRows) (fieldRows +1) fieldrev)

            setField (tail  (init newField)) g


        else do
            ---padding
            let newField = rowsAttack PlayerA (fieldRows) (fieldRows +1) ([firstRowNew] ++ field ++ [initRow])

            setField (tail  (init newField)) g


-- | recursively traverse field, compare each two adjacent rows (injective) and move units corresponding to player and other units on the field         
rowsAttack :: PlayerID -> Int -> Int -> Field -> Field
-- let units at the end of the field disappear
rowsAttack pId (-1) _ field = replaceInList 0 (fst res) field where
    res = (field!!0, (replicate fieldRows Nothing)) --unitsAttack pId ((field!!0), (replicate fieldRows Nothing))
-- if the end is not reached, continue
rowsAttack pId x1 x2 field = do
    rowsAttack pId (x1-1)(x2-1) (replaceInList x2 (snd res) (replaceInList x1 (fst res) field)) where
        res = unitsAttack pId ((field!!x1), (field!!x2))


-- | takes the whole field, inclusive padding rows (bench to be put on field and nothings at the end) and compares each 2 rows next to each other, to move move and compete units. Only attacking player's units move
--           attacking player -> (row1 unit, row2 unit) -> (row 1 result (unit/nothing), row 2 result(unit/nothing))
unitsAttack :: PlayerID -> ([Maybe Unit],[Maybe Unit]) -> ([Maybe Unit] , [Maybe Unit])

--endCondition
unitsAttack pId ([], []) = ([], [])

-- empty fields get returned as such
unitsAttack pId (Nothing:us, Nothing:vs) = (Nothing:(fst nxt), Nothing:(snd nxt)) where
    nxt = unitsAttack pId (us, vs)

--take a step
unitsAttack pId (u:us, Nothing:vs) | view uStrength (fromJust u) <= 0   = (Nothing:(fst nxt), Nothing:(snd nxt))
                                   | (view uPlayer (fromJust u)) == pId = (Nothing:(fst nxt),       u:(snd nxt))
                                   | otherwise                          = (u:(fst nxt),       Nothing:(snd nxt))
                                where nxt = unitsAttack pId (us, vs)

-- do nothing
unitsAttack pId (Nothing:us, v:vs) = (Nothing:(fst nxt), v:(snd nxt)) where
    nxt = unitsAttack pId (us, vs)

--battle
unitsAttack pId (u:us, v:vs)       | view uStrength (fromJust u) <= 0                         = (Nothing:(fst nxt)      , v:(snd nxt))
                                   | view uPlayer (fromJust u)   /= view uPlayer (fromJust v) = ((fst result):(fst nxt) , (snd result):(snd nxt) )
                                   | otherwise                                                = (u:(fst nxt)            , v:(snd nxt))
                                where result = compete (fromJust u, fromJust v)
                                      nxt = unitsAttack pId (us, vs)
-- default case
unitsAttack _ _  = ([], [])


-- | Compares an attacking unit with a defending unit and returns a tuple of maybe units as result
compete :: (Unit, Unit) -> (Maybe Unit, Maybe Unit)
compete (u, v) | sU == sV = (Nothing , deadUnit)                                 -- units cancel each other, add dead Unit for drawing
               | sU <  sV = (deadUnit, Just(set uStrength (sV - sU ) v ))        -- attacking a stronger unit: die and do str as damage
               | sU >  sV = (Nothing , Just u)                                   -- attacking weaker unit: replace unit
               | otherwise = (Nothing, Nothing)                                  -- default case
               where sU = view uStrength u
                     sV = view uStrength v


-- Unit Getter

-- | return the 'uStrength' component for a given 'Unit'
getUnitStrength :: Unit -> Int
getUnitStrength u = view uStrength u

-- | return the 'uPlayer' component for a given 'Unit'
getUnitPlayer :: Unit -> PlayerID
getUnitPlayer u = view uPlayer u

-- Unit Setter

-- | set the 'uStrength' component of the given 'Unit' to the given number
setUnitStrength :: Int -> Unit -> Unit
setUnitStrength strength unit = set uStrength strength unit


-- ------------------------------------
-- --------- CARD FUNCTIONS -----------
-- ------------------------------------

-- | DOPPELT GEMOPPELT card function
doppeltGemoppelt :: FieldRow -> FieldRow
doppeltGemoppelt fr =  take fieldRows (a fr ++ a fr ++ (replicate fieldRows Nothing)) where
    a fr = filterUnits fr

-- | BITTE EIN BISHIFT card function
--                CardVariation    Row        Row
bitteEinBitShift :: ([Int],[Int]) -> FieldRow -> FieldRow
bitteEinBitShift (var1, _) fr = do
    let shifted = bEBSi (head var1) (filterUnits fr)
    padRow shifted

-- | helper function for bitteEinBitShift
-- ShiftAmount    Row         Row
bEBSi :: Int -> FieldRow -> FieldRow
bEBSi 0 fr = fr
bEBSi bits (f:fr) = bEBSi (bits -1) (fr ++ [f])
bEBSi _    _      = []

-- | 7ZIP card function
sevenZip :: Int -> Maybe Unit ->  FieldRow -> FieldRow
sevenZip accu current fr = padRow $ tail $ (sevenZipHelper accu current (filterUnits fr))
sevenZipHelper :: Int -> Maybe Unit ->  FieldRow -> FieldRow
sevenZipHelper accu current []                    = [(Just Unit{_uStrength = accu,_uPlayer = view uPlayer (fromJust current), _hasAttacked = False})]
sevenZipHelper accu current (f:fr) | f == current = sevenZipHelper (accu + (view uStrength (fromJust current))) f fr
                                   | otherwise    = [(Just Unit{_uStrength = accu,_uPlayer = view uPlayer (fromJust current), _hasAttacked = False})] ++ sevenZipHelper (0 + (view uStrength (fromJust f))) f fr


-- | SPIEGLEIN AN DER WAND card function
spiegleinAnDerWand :: FieldRow -> FieldRow
spiegleinAnDerWand fr = (padRow . reverse . filterUnits) fr

-- | HOCH (ODER RUNTER) DIE HÄNDE card function
hochOderRunter :: Int -> ([Int],[Int]) -> FieldRow -> FieldRow
hochOderRunter randSeed (buffs:b, nerfs:n)   fr  = do
    let range = (length (filterUnits fr)) -1 :: Int
    let rand  = randomRs(0, range) (mkStdGen (randSeed+93))
    let var   = ((take buffs rand), take nerfs (drop buffs rand))
    let result = map Just (((applyUpDown (fst var) 1) . (applyUpDown (snd var) (-1)). (map fromJust)) (filterUnits fr))
    padRow result
    where applyUpDown [] int fr = fr
          applyUpDown (a:as) int fr = applyUpDown as int ((fst spl) ++ [( over uStrength ((max 0).(+int)) (fr!!a))] ++ tail (snd spl))
            where spl = splitAt a fr
hochOderRunter _ _ _ = [] 




-- | HAST DU MAL NE KIPPE? card function
kippe :: FieldRow -> FieldRow
kippe fieldRow = take fieldRows ((map ((Just) . (over uStrength kippeHelper) . fromJust) (filterUnits fieldRow)) ++ (replicate fieldRows Nothing)) where
    kippeHelper x | x > 1 = x
                  | x == 0 = 1
                  | x == 1 = 0
                  | otherwise = -1

-- | fill up a FieldRow with enough Nothings to generate a "full" FieldRow
padRow :: FieldRow -> FieldRow
padRow fr = take fieldRows (fr ++ repeat Nothing)

-- | ICH MUSS AUFS KLON card function variant
klon :: PlayerID -> FieldRow -> FieldRow
klon player fieldRow = do
    let reducedList = filterUnits fieldRow
    reducedList ++ (replicate 12 (Just Unit{_uStrength = 0,_uPlayer = player, _hasAttacked = False}))

-- | ICH MUSS AUFS KLON card function variant: takes a randSeed, variation v1 and the row. Randomly chooses 2 unit and appends them
klon2 :: Int -> ([Int],[Int]) -> FieldRow -> FieldRow
klon2 randSeed ([0],_)  fr = fr
klon2 randSeed (v:v1,_) fr = let units = filterUnits fr in do
                        let range = ((length units) -1 ) :: Int
                        let rand  = randomRs(0, range) (mkStdGen (randSeed+57))
                        (klon2 randSeed ([v - 1], []) units) ++  [(units!!(rand!!v))]
klon2 _ _ fr = fr

-- | add v1 units of v2 strength     
flood :: PlayerID -> ([Int],[Int]) -> FieldRow -> FieldRow
flood pId ([v1],[v2]) fr = padRow ((filterUnits fr) ++ replicate v1 (Just Unit{_uStrength = v2,_uPlayer = pId, _hasAttacked = False}))
flood _   _           _  = []

-- | PIGSORT card function
pigSort :: FieldRow -> FieldRow
pigSort fieldRow = padRow (sortBy ps (filterUnits fieldRow)) where
    ps Nothing u = LT
    ps u Nothing = GT
    ps u1 u2 = compare u1 u2

-- | return the strength of a cell: a low negative default value if no unit is on this cell or the actual unit strength otherwise
cellStrength :: FieldCell -> Int
cellStrength cell = gs cell where
    gs Nothing = -99
    gs (Just u) = view uStrength u

-- | KOMBI-NIERE card function. Takes the current playerID and a variation combination of unit strengths v1 and v2. If this combination is found in a row, it will be replaced by a stronger combination given in v2
--            CardVariation  -> bench -> res
kombiNiere :: PlayerID -> ([Int], [Int]) -> FieldRow -> FieldRow
kombiNiere pId (v1, v2) fr  | length v1 > length fr                     = fr ++ (replicate fieldRows Nothing )                                                         -- cant use comparision, return
                            | v1 == unitsToInts (fst splt)               = (map (generateUnit pId) v2) ++ (kombiNiere pId (v1, v2) (snd splt)) -- replace with unit if pattern is found
                            | otherwise                                  = [(head fr)] ++ (kombiNiere pId (v1, v2) (tail fr))
                            where splt                = splitAt (length v1) fr
                                  unitsToInts fr =  (map (getUnitStrength . (fromMaybe (fromJust deadUnit))) fr)

-- | calculates a combination to be given as input variation for the kombiNiere Function
-- Unit strength as Int, random values [0.00 .. 0.99 ::Float] [Output strength] [Output]
calcCombOut :: Int -> [Float] -> [Int] -> [Int]
calcCombOut str (r:rnd) (o:out) | str <= 0  = (o:out)
                                | r > 0.5   = calcCombOut (str -1) rnd (out++[(o + 1)])
                                | r < 0.5   = calcCombOut (str -1) rnd ((o + 1):out )
                                | otherwise = []
calcCombOut _   _       _                   = []                         

-- | removes nothings from fieldrow
filterUnits :: FieldRow -> FieldRow
filterUnits (Nothing:xs) = filterUnits xs
filterUnits []           = []
filterUnits (x:xs)       = [x] ++ filterUnits xs


-- | returns only the Units from a given 'FieldRow' that belong to the given 'PlayerID'. Pass (switch pId) instead of a pId to get all Units *except* those from the given player.
onlyPlayerUnitsFromRow :: PlayerID -> FieldRow -> FieldRow
onlyPlayerUnitsFromRow pId fr = fmap extractor fr where
    extractor Nothing = Nothing
    extractor (Just u) = if matchUnitToPlayer pId u then Just u else Nothing


-- | returns whether the given 'Unit' belongs to the player with the given 'PlayerID'
matchUnitToPlayer :: PlayerID -> Unit -> Bool
matchUnitToPlayer pId u = getUnitPlayer u == pId

-- Other functions

-- | Generates a fieldCell unit for a playerId with strength
generateUnit :: PlayerID -> Int -> FieldCell
generateUnit pId str = Just Unit{
    _uStrength = str,
    _uPlayer = pId,
    _hasAttacked = False
}

--  | generates a card. given a random seed playerID and cardID, which dictates the type. Generated cards with random variation if possible
generateCard :: Int -> PlayerID -> CardID -> Card
generateCard randSeed playerId cardid = case cardid of
    -- Doppelt Gemoppelt
    Card1  -> do
        Card{
        _cId = Card1,
        _cPlayer = playerId,
        _cName = "",
        _cFnct = doppeltGemoppelt,

        _cVars = ([],[]),
        _cInfoOffset = Nothing,
        _listPosition = 0
        }
    -- Place Units
    Card2  -> do
        let range = 4.2 :: Float
        let rand  = floor (fst(randomR(0, range) (mkStdGen (randSeed+67))) )

        let var = ([amountOf rand],[rand] )

        Card{
        _cId = Card2,
        _cPlayer = playerId,
        _cName = "",
        _cFnct = flood playerId var,
        _cVars = var,
        _cInfoOffset = Just ((110   ,102),(115,143)),
        _listPosition = 0
        }
        where amountOf s | s == 4 = 1
                         | s == 3 = 1
                         | s == 2 = 2
                         | s == 1 = 3
                         | s == 0 = 4
                         | otherwise = 1

    -- Kippen                     
    Card3  -> do
        Card{
        _cId = Card3,
        _cPlayer = playerId,
        _cName = "",
        _cFnct = kippe,
        _cVars = ([],[]),
        _cInfoOffset = Nothing,
        _listPosition = 0
        }
    -- Klonen
    Card4  -> do
        let var = ([2],[])
        Card{
        _cId = Card4,
        _cPlayer = playerId,
        _cName = "",
        _cFnct = padRow . (klon2 randSeed var),
        _cVars = ([],[]),
        _cInfoOffset = Nothing,
        _listPosition = 0
        }
    -- Hoch oder Runter
    Card5  -> do

        let range = 0.99 :: Float
        let rand  = randomRs(0, range) (mkStdGen (randSeed+93))


        let maxBuffs = floor ((rand!!0) * 5 + 1)
        let maxNerfs = maxBuffs - 2

        let var = ([maxBuffs],[max maxNerfs 0 ])

        Card{
        _cId = Card5,
        _cPlayer = playerId,
        _cName = "",
        _cFnct = (hochOderRunter randSeed var),
        _cVars = var,
        _cInfoOffset = Just ((43,121),(130,161)),
        _listPosition = 0
        }
        -- Spiegeln
    Card6  -> do
        Card{
        _cId = Card6,
        _cPlayer = playerId,
        _cName = "",
        _cFnct = spiegleinAnDerWand,
        _cVars = ([],[]),
        _cInfoOffset = Nothing,
        _listPosition = 0
        }
        -- sevenZip
    Card7  -> do
        let initUnit = (Just Unit{_uStrength = -1,_uPlayer = playerId, _hasAttacked = False})
        Card{
        _cId = Card7,
        _cPlayer = playerId,
        _cName = "",
        _cFnct = sevenZip 0 initUnit,
        _cVars = ([],[]),
        _cInfoOffset = Nothing,
        _listPosition = 0
        }
        -- Kombinieren
    Card8  -> do
        let range = 0.99 :: Float
        let rand = randomRs(0, range) (mkStdGen (randSeed+71))   -- 71 arbitrary primenumber
        let combinationSizeIn = floor(1.4 + ((rand!!0) * 3)) :: Int
        let combinationSizeOut = floor(1 + ((rand!!1) * 2)) :: Int


        let combIn = take combinationSizeIn [combDistrIn x | x <- snd(splitAt 2 rand) ] where -- generate the list of units, that is to be found in the pbench
                    combDistrIn x | x >= 0.95 = 2
                                  | x >= 0.57 = 1
                                  | x <  0.57 = 0
                                  | otherwise  = 0


        let combOutStr = (sizeBonus combinationSizeIn) + (foldr (+) 0 combIn )      -- weigh strength of output by difficulty to find(length) and strength of units in comb

        let combOut = calcCombOut combOutStr (snd(splitAt (2 + combinationSizeIn)  rand)) (take combinationSizeOut [0..])



        let var = sortOutIdentical(combIn,combOut)
        Card{
        _cId = Card8,
        _cPlayer = playerId,
        _cName = "",
        _cFnct = (take fieldRows) . (kombiNiere playerId var),
        _cVars = var,
        _cInfoOffset = Just ((39,155),(109,155)),
        _listPosition = 0
        }
        where sortOutIdentical (v1,v2) | v1 == v2  = (v1,[0] ++ v2)
                                       | otherwise = (v1,v2)
              sizeBonus s | s <= 0 = 0
                          | s == 1 = 0
                          | s == 2 = 1
                          | s == 3 = 3
                          | s > 3  = s + 2
                          | otherwise = 5
              maxStrengthBonus b | any (> 2) b = 3
                                 | any (==2) b = 1
                                 |otherwise    = 0

    -- Sortieren
    Card9  -> do
        Card{
        _cId = Card9,
        _cPlayer = playerId,
        _cName = "",
        _cFnct = pigSort,
        _cVars = ([],[]),
        _cInfoOffset = Nothing,
        _listPosition = 0
        }
    -- BitShift
    Card10 -> do
        let range = 2 :: Int
        let rand = randomR(1, range) (mkStdGen (randSeed+89))   -- 89 arbitrary primenumber
        let var = ([1],[])                                      -- possibility to variate randomly, but always 1 is cooler
        Card{
        _cId = Card10,
        _cPlayer = playerId,
        _cName = "",
        _cFnct = bitteEinBitShift var,
        _cVars = ([],[]),
        _cInfoOffset = Nothing,
        _listPosition = 0
        }
    -- None
    Card_None -> do
        Card{
        _cId = Card_None,
        _cPlayer = playerId,
        _cName = "",
        _cFnct = id,
         _cVars = ([],[]),
        _cInfoOffset = Nothing,
        _listPosition = 0
        }


-- | quick out-of-bounds checking
outOfBounds :: (Int , Int) -> Bool
outOfBounds (x,y) = or [(x >= fieldColumns),(x<0),(y<0),(y>=fieldRows)]

-- | Returns Unit on the Field
scanField :: (Int,Int) -> Field -> Maybe Unit
scanField  (x,y) field | outOfBounds (x,y) = Nothing
                       | otherwise         = scan ((field!!x)!!y) where
    scan Nothing  = Nothing
    scan (Just u) = Just u

-- | takes position, unit and field and replaces the cell at coords with unit
setFieldCell :: (Int, Int) -> Maybe Unit -> Field -> Field
setFieldCell (x,y) unit field | outOfBounds(x,y) = field
                              | otherwise = replaceInList x (replaceInList y unit (field!!y)) field

-- | setting specific fields in context of complex attacks 
setField :: Field -> Game -> Game
setField f g = set field f g

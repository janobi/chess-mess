{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- ignore the "Use camelCase" warning from the Haskell Language Server to use underscores
{-# HLINT ignore "Use camelCase" #-}


module TypeDefs where

import Control.Lens hiding ((#))
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Utils
import ASTBuilders

-- View type declarations
type ImgMap = [(String, Element)]
type Clickable = (String, Rect, [GameState], [PlayerID])

data Rect = Rect {
    _rPosition :: UI.Point,
    _rSize :: UI.Point
}

data CustomRand = CustomRand {
    _current :: [Float],
    _following :: [Float]
}

-- Model type declarations
data GameState = GS_NotStarted
               | GS_WaitingForPlayer
               | GS_ExecuteRound
               | GS_Finished
               deriving (Show,Eq)


data PlayerID = PlayerA | PlayerB
    deriving Eq

instance Show PlayerID where
    show PlayerA = "Spieler 1"
    show PlayerB = "Spieler 2"

-- data CardID = Card1 | ... | Card10 | Card_None deriving Eq
$(genNumberedDataType "CardID" "Card" 10 True [''Eq])

instance Show CardID where
    show Card1 = "Doppelt-Gemoppelt"
    show Card2 = "Ed's Units"
    show Card3 = "Hast du mal ne Kippe?"
    show Card4 = "Ich muss aufs Klo(n)"
    show Card5 = "Hoch (oder runter) die Hände"
    show Card6 = "Spieglein an der Wand"
    show Card7 = "7-Zip"
    show Card8 = "Kombi-Niere"
    show Card9 = "PigSort"
    show Card10 = "Bitte ein BitShift"
    show Card_None = ""

-- | Type to handle static and variable card information, like function, player, name etc.
data Card = Card {
    _cId :: CardID,
    _cPlayer :: PlayerID,
    _cName :: String,
    _cFnct :: CardFunction,
    _cVars :: ([Int],[Int]),
    _cInfoOffset :: Maybe (UI.Point,UI.Point),
    _listPosition :: Int
}

type Highscore = (String, Int)
type Highscores = [Highscore]

-- | A Type that can change a FieldRow
class FieldRowManipulator a where
    modFieldRow :: a -> FieldRow -> FieldRow

    modFieldRow a fr = fr

instance FieldRowManipulator CardID where --old framework card functions, ultimately abandoned approach but still here for reference
    modFieldRow Card1 fr = fr
    modFieldRow Card2 fr = fr
    modFieldRow Card3 fr = fr
    modFieldRow Card4 fr = fr
    modFieldRow Card5 fr = fr
    modFieldRow Card6 fr = fr
    modFieldRow Card7 fr = fr
    modFieldRow Card8 fr = fr
    modFieldRow Card9 fr = fr
    modFieldRow Card10 fr = fr
    modFieldRow Card_None fr = fr


type CardFunction = (FieldRow -> FieldRow)
     

-- | Contains game information relevant to players each in record syntax, e.g. health, cards 
data Player = Player {
    _pId :: PlayerID,
    _pHealth :: Int,
    _pCards :: [Card],
    _pBench :: FieldRow,
    _pActiveCard :: Card
} 

-- | Contains game information relevant to units on the field and bench, e.g. strength, corresponding player 
data Unit = Unit {
    _uStrength :: Int,
    _uPlayer :: PlayerID,
    _hasAttacked :: Bool
} deriving (Show,Eq)

-- type defs for readability
type FieldCell = Maybe Unit
type FieldRow = [FieldCell]
type Field = [FieldRow]

-- | all data for the current state of the game in record syntax, instantiated in the game variable
data Game = Game {
    _state :: GameState,
    _activePlayer :: PlayerID,
    _players :: [Player],
    _field :: Field,
    _rounds :: Int,
    _waitingOpID :: String,
    _playableCardsLeft :: Int
}


-- build lenses for the relevant types
makeLenses ''Card
makeLenses ''Player
makeLenses ''Unit
makeLenses ''Game
makeLenses ''Rect


-- instantiate units as ord for easier comparison in card functions
instance Ord Unit where
  u1 <= u2 = view uStrength u1 <= view uStrength u2


-- new type classes & their instances

-- | A type that stores position and size data so that hover checks are possible
class Shape a where
    hovering :: UI.Point -> a -> Bool
    getPos :: a -> UI.Point
    getSize :: a -> UI.Point
    getCenter :: a -> UI.Point

    hovering p a = isPointInRect p topLeft bottomRight where
        topLeft = getPos a
        size = getSize a
        bottomRight = getBottomRightPoint topLeft size

    getCenter a = getRectCenter (getPos a) (getSize a)

instance Shape Rect where
    getPos r = view rPosition r
    getSize r = view rSize r


-- | A type with two values that can be switched
class Switchable a where
    switch :: a -> a

instance Switchable PlayerID where
    switch PlayerA = PlayerB
    switch PlayerB = PlayerA

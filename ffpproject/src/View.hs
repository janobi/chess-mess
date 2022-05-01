{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
-- ignore the "Use camelCase" warning from the Haskell Language Server to use underscores
{-# HLINT ignore "Use camelCase" #-}

module View
    ( serveGUI
    ) where

-- imports for general functions, monads etc
import Control.Monad
import Data.IORef
import System.Random
import Control.Exception
import Data.List

-- Threepenny imports
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- in-project imports
import qualified Model
import Utils
import TypeDefs
import CanvasAdds as UI2
import ASTBuilders


-- | load a Threepenny config and serve the GUI
serveGUI :: IO ()
serveGUI = do
    let config = defaultConfig {
        jsStatic = Just "src/assets" --serve anything in the assets folder under a /static/ URL path, used to load custom CSS to make styling easier and more centralized
        }
    startGUI config setup

-- width and height of the canvases in px
canvasWidth = 1280
referenceWidth = fromIntegral canvasWidth :: Double
canvasHeight = 650
referenceHeight = fromIntegral canvasHeight :: Double

-- board position & size parameters
chessboardPos = (referenceWidth*0.2855, 9)
player0benchPos = (referenceWidth*0.22, 9)
player1benchPos = (referenceWidth*0.746, 9)
squareWidth = 46

-- Button Rectangles & Position Parameters
playButtonRect = Rect (referenceWidth/2-160/2,440) (160,42)
playButtonWidth = 160
playButtonHeight = 42
playAgainButtonRect = Rect (referenceWidth/2-235/2,490) (235,42)
execButtonRect = Rect (referenceWidth/2-230/2,590) (230,55)
playCardButtonSize = (50,62)
playCardButtonGap = 20
playCardARect = Rect (x player0benchPos - playCardButtonGap - x playCardButtonSize,140) playCardButtonSize
playCardBRect = Rect (x player1benchPos + squareWidth + playCardButtonGap,140) playCardButtonSize


-- PLAYER TITLE & CARDS POSITIONING

-- Distance X from left side for list positioning (PlayerA)
playerACardsOffsetX = 0
-- Distance Y from top for list positioning 
globalplayerCardsOffsetY = 320
-- Offsetting rect and text positioning to achieve a correct overlapping (> font size to have more click surface on top side of titles)
rectPlayerCardOffsetY = globalplayerCardsOffsetY - 15
-- distance between card titles in list
playerCardsStepY = 25
-- Distance X from left side for post positioning (PlayerB)
playerBCardsOffsetX = referenceWidth - x playerCardsTitleSize
-- "Karten Spieler 1/2" title sizes
playerCardsTitleSize = (250,44)
-- Clickable rect sizes for card list (font size = 14 -> 14 + 2 = 16 for slightly bigger click surface)
playerCardsRectSize = (250, 16)
-- "Karten Spieler 1/2" title distance Y from top
playerCardsOffsetY = 5
-- Centering card list titles (PlayerA)
playerACenterX = playerACardsOffsetX + x playerCardsTitleSize / 2
-- Centering card list titles (PlayerB)
playerBCenterX = playerBCardsOffsetX + x playerCardsTitleSize / 2
-- Size of one big preview image of a card
playerCardsPreviewSize = (160,230)
-- Distance Y from top for displaying the preview image
playerCardsPreviewOffsetY = 63
-- Preview card rect size (PlayerA)
cardPreviewLeftRect = Rect (playerACenterX - x playerCardsPreviewSize / 2, playerCardsPreviewOffsetY) playerCardsPreviewSize
-- Preview card rect size (PlayerB)
cardPreviewRightRect = Rect (playerBCenterX - x playerCardsPreviewSize / 2, playerCardsPreviewOffsetY) playerCardsPreviewSize

-- List click RECTANGLES
cardRectanglesLeft  = take Model.cardAmount [Rect(playerACardsOffsetX, rectPlayerCardOffsetY+(playerCardsStepY * n )) playerCardsRectSize | n <- [0..]]
cardRectanglesRight = take Model.cardAmount [Rect(playerBCardsOffsetX, rectPlayerCardOffsetY+(playerCardsStepY * n )) playerCardsRectSize | n <- [0..]]

-- number of the relevant highscores
highScoreLimit = 5

-- clickable areas as 4-tuples of a button ID, a Rect with its position and size, a list of conditional gameStates and a liste of conditional activePlayer PlayerIDs
clickAreas :: [Clickable]
clickAreas = [
        ("playCardA", playCardARect, [GS_WaitingForPlayer], [PlayerA]),
        ("playCardB", playCardBRect, [GS_WaitingForPlayer], [PlayerB]),
        ("ExecuteRound", execButtonRect, [GS_WaitingForPlayer], [PlayerA, PlayerB]),
        ("playButton", playButtonRect, [GS_NotStarted], [PlayerA, PlayerB]),
        ("playAgainButton", playAgainButtonRect, [GS_Finished], [PlayerA, PlayerB])
        ] ++
        -- left and right player cards
        (take Model.cardAmount [("card" ++ show n ++ "left",  cardRectanglesLeft!!(n -1)  , [GS_WaitingForPlayer], [PlayerA]) | n <- [1,2..]]) ++
        (take Model.cardAmount [("card" ++ show n ++ "right", cardRectanglesRight!!(n -1) , [GS_WaitingForPlayer], [PlayerB]) | n <- [1,2..]])

-- | get random
getRandom :: UI Int
getRandom = randomIO :: UI Int

-- | create the GUI
setup :: Window -> UI ()
setup window = do
    -- set page title
    return window # set title "CHESS-MESS!"

    -- add custom CSS
    UI.addStyleSheet window "style.css"

    -- create HTML elements
    wrapper <- mkElement "div"
        # set UI.id_ "mainWrapper"

    canvasWrapper <- mkElement "div"
        # set UI.id_ "canvasWrapper"

    canvasBG <- UI.canvas
        # set UI.width canvasWidth
        # set UI.height canvasHeight
        # set UI.id_ "canvasBG"

    canvasFG <- UI.canvas
        # set UI.width canvasWidth
        # set UI.height canvasHeight
        # set UI.id_ "canvasFG"

    buttonRow <- mkElement "div"
        # set UI.id_ "hsButtonRow"
        # set UI.class_ "hidden"

    brLabel <- mkElement "label"
        # set UI.text "Trage hier Deinen Namen ein:"

    brInput <- mkElement "input"
        # set UI.id_ "hsInput"
        # set UI.type_ "text"
        # set UI.value "Cooler Dude"

    brButton <- mkElement "button"
        # set UI.type_ "button"
        # set UI.text "Ab in die Highscores!"

    desc <- mkElement "div"
        # set UI.id_ "description"
        # set text "Was ist Chess-Mess überhaupt? Erzeuge oder vermehre Einheiten durch das sinnvolle Einsetzen einer oder mehrerer Karte/n. Erarbeite dir hierdurch taktische Vorteile. Du besitzt insgesamt 11 Karten pro Runde, klicke auf einen Kartennamen deiner Liste und sie wird dir in groß angezeigt. Klicke den Pfeil neben der Kartenvorschau und du kannst sie auf deine generierten Einheiten in der separaten Reihe vor dem Spielfeld anwenden. Beachte: Du darfst maximal 4 Karten pro Runde spielen. Eine Erklärung der Funktionsweise ist auf der jeweiligen Karte nachzulesen. Den Abschluss deiner Runde musst du durch Klicken des Runde-beenden-Buttons bestätigen. Deine generierten Einheiten wandern somit eine Reihe weiter zum Gegner. Durch die unterschiedlichen Einheiten-Stärken beim Aufeinandertreffen mit gegnerischen Einheiten gewinnst oder verlierst du deinen Platz auf dem jeweiligen Schachfeld. Die Einheiten, die zu der Basis deines Gegners gelangen, ziehen diesem entsprechend ihrer verbleibenden Stärke Leben ab. Hat dein Gegner kein Leben mehr, gewinnst du. Viel Erfolg!"

    hiddenButton <- UI.button
        # set UI.type_ "button"
        # set UI.id_ "hiddenButton"

    -- integrate HTML elements
    element canvasWrapper #+ [
        element canvasBG,
        element canvasFG
        ]

    element buttonRow #+ [
        element brLabel,
        element brInput,
        element brButton
        ]

    element wrapper #+ [
        element canvasWrapper,
        element buttonRow,
        element desc
        ]

    getBody window #+ [
        element wrapper,
        element hiddenButton
        ]

    -- load images
    images <- sequence [
        -- Chessboard
        createPNG "bg" "schachbrett55012x12.png",
        -- Cards
        createPNG "Doppelt-Gemoppelt" "KarteSmallDoppeltGemoppelt.png",
        createPNG "7-Zip" "KarteSmall7Zip.png",
        createPNG "Ed's Units" "KarteSmallNullHochNull.png",
        createPNG "Spieglein an der Wand" "KarteSmallSpieglein.png",
        createPNG "Bitte ein BitShift" "KarteSmallBitShift.png",
        createPNG "Hoch (oder runter) die Hände" "KarteSmallHochDieHaende.png",
        createPNG "Hast du mal ne Kippe?" "KarteSmallKippe.png",
        createPNG "Ich muss aufs Klo(n)" "KarteSmallKlon.png",
        createPNG "PigSort" "KarteSmallPigSort.png",
        createPNG "Kombi-Niere" "KarteSmallKombiNiere1.png",
        -- Fonts etc.
        createPNG "title" "ChessMessTitle.png",
        createPNG "titlePlayerOne" "SpielkartenP1.png",
        createPNG "titlePlayerTwo" "SpielkartenP2.png",
        createPNG "playleft" "PlayLeftColor.png",
        createPNG "playright" "PlayRightColor.png",
        createPNG "endRoundP1" "ButtonPlayerOneEndRound.png",
        createPNG "endRoundP2" "ButtonPlayerTwoEndRound.png"
        ]

    -- canvas drawing settings
    element canvasBG
        # set UI.textAlign UI.Center
        # set UI.strokeStyle "black"
    element canvasFG
        # set UI.textAlign UI.Center
        # set UI.strokeStyle "black"

    -- create IORef for mutable game state
    gameRef <- liftIO $ newIORef Model.initGame

    -- draw a primitive loading screen first. One second later, load the splash screen. This is to give images that should be drawn on the splash screen enough time to load since Threepenny doesn't have a load event for images.
    drawLoadingScreen canvasBG
    scheduleWaitingOp "LoadGame" 1000 gameRef

    -- event handlers
    -- check if a waiting operation is due (MacGyvered via a native JS setTimeout function built in js_delayedLoading that schedules an automated click on a hidden HTML button which we wait for via a click EventListener)
    on UI.click hiddenButton $ const $ do
        game <- liftIO $ readIORef gameRef

        case Model.getWaitingOpID game of
            "LoadGame" -> drawSplashScreen canvasBG canvasFG images buttonRow gameRef False
            _ -> return ()

        liftIO $ modifyIORef gameRef Model.resetWaitingOpID
        return ()

    on UI.click brButton $ const $ do
        -- get the previous highscores
        scores <- getHighscores

        -- collect the new name and score
        newName <- brInput # get UI.value
        game <- liftIO $ readIORef gameRef
        let newScore = Model.getRoundCounter game

        -- write the new highscore to the highscores file. The order is irrelevant since the scores gets sorted on the next read anyways, so we can use Cons here
        writeHighscores ((newName, newScore):scores)

        -- hide the buttonRow
        element buttonRow # set UI.class_ "hidden"

        -- draw the updated splashscreen
        drawSplashScreen canvasBG canvasFG images buttonRow gameRef False

        return ()

    -- check for canvas mouse-hovers
    on UI.mousemove canvasFG $ \coords -> do
        -- check the gameState and only react if needed
        game <- liftIO $ readIORef gameRef

        -- check if the mouse hovers over a clickable area in the right context
        let hit = getHit coords game clickAreas

        -- set the cursor accordingly
        case hit of
            Nothing -> element canvasFG # set UI.class_ ""
            Just _ -> element canvasFG # set UI.class_ "clickable"

        return ()

    -- check for canvas mouseclicks (can't use the click event because it doesn't return the click coordinates)
    on UI.mousedown canvasFG $ \coords -> do
        game <- liftIO $ readIORef gameRef

        -- check if the mouse hovers over a clickable area in the right context
        let hit = getHit coords game clickAreas

        -- react accordingly
        let activePlayer = Model.getActivePlayer game
        let currentPlayerCardList = Model.getPCards activePlayer game

        gameROM <- liftIO $ readIORef gameRef
        case hit of
            Nothing -> return ()
            Just "playButton" -> startGame canvasBG canvasFG images gameRef
            Just "playAgainButton" -> do
                -- hide the highscore button in case it was shown but ignored
                element buttonRow # set UI.class_ "hidden"

                -- restart Game
                startGame canvasBG canvasFG images gameRef
            Just "playCardA" -> do
                if (Model.getPlayableCardsLeft gameROM <= 0) then do
                    return()
                else do
                    rand5 <- getRandom
                    let activeCard = Model.getActivePCard activePlayer game
                    debugLog (Model.getCardId activeCard)
                    liftIO $ modifyIORef gameRef $ Model.playCard activeCard--(modFieldRow (Model.getCardFnct activeCard))
                    -- Replace played card with Card_None
                    let newPlayerCardList = Utils.replaceInList (Model.getCardPosition activeCard) (Model.generateCard 0 activePlayer Card_None) currentPlayerCardList
                    -- Create new card list
                    liftIO $ modifyIORef gameRef $ Model.setPCards activePlayer newPlayerCardList
                    drawGame canvasFG images gameRef
            Just "playCardB" -> do
                if (Model.getPlayableCardsLeft gameROM <= 0) then do
                    return()
                else do
                    rand6 <- getRandom
                    let activeCard = Model.getActivePCard activePlayer game
                    debugLog (Model.getCardId activeCard)
                    liftIO $ modifyIORef gameRef $ Model.playCard activeCard--(modFieldRow activeCard)
                    -- Replace played card with Card_None
                    let newPlayerCardList = Utils.replaceInList (Model.getCardPosition activeCard) (Model.generateCard 0 activePlayer Card_None) currentPlayerCardList
                    -- Create new card list
                    liftIO $ modifyIORef gameRef $ Model.setPCards activePlayer newPlayerCardList
                    drawGame canvasFG images gameRef

            -- PLAYER A
            Just "card1left"   -> drawPreviewImage currentPlayerCardList activePlayer  0 images canvasFG gameRef
            Just "card2left"   -> drawPreviewImage currentPlayerCardList activePlayer  1 images canvasFG gameRef
            Just "card3left"   -> drawPreviewImage currentPlayerCardList activePlayer  2 images canvasFG gameRef
            Just "card4left"   -> drawPreviewImage currentPlayerCardList activePlayer  3 images canvasFG gameRef
            Just "card5left"   -> drawPreviewImage currentPlayerCardList activePlayer  4 images canvasFG gameRef
            Just "card6left"   -> drawPreviewImage currentPlayerCardList activePlayer  5 images canvasFG gameRef
            Just "card7left"   -> drawPreviewImage currentPlayerCardList activePlayer  6 images canvasFG gameRef
            Just "card8left"   -> drawPreviewImage currentPlayerCardList activePlayer  7 images canvasFG gameRef
            Just "card9left"   -> drawPreviewImage currentPlayerCardList activePlayer  8 images canvasFG gameRef
            Just "card10left"  -> drawPreviewImage currentPlayerCardList activePlayer  9 images canvasFG gameRef
            Just "card11left"  -> drawPreviewImage currentPlayerCardList activePlayer 10 images canvasFG gameRef
            -- PLAYER B
            Just "card1right"  -> drawPreviewImage currentPlayerCardList activePlayer  0 images canvasFG gameRef
            Just "card2right"  -> drawPreviewImage currentPlayerCardList activePlayer  1 images canvasFG gameRef
            Just "card3right"  -> drawPreviewImage currentPlayerCardList activePlayer  2 images canvasFG gameRef
            Just "card4right"  -> drawPreviewImage currentPlayerCardList activePlayer  3 images canvasFG gameRef
            Just "card5right"  -> drawPreviewImage currentPlayerCardList activePlayer  4 images canvasFG gameRef
            Just "card6right"  -> drawPreviewImage currentPlayerCardList activePlayer  5 images canvasFG gameRef
            Just "card7right"  -> drawPreviewImage currentPlayerCardList activePlayer  6 images canvasFG gameRef
            Just "card8right"  -> drawPreviewImage currentPlayerCardList activePlayer  7 images canvasFG gameRef
            Just "card9right"  -> drawPreviewImage currentPlayerCardList activePlayer  8 images canvasFG gameRef
            Just "card10right" -> drawPreviewImage currentPlayerCardList activePlayer  9 images canvasFG gameRef
            Just "card11right" -> drawPreviewImage currentPlayerCardList activePlayer 10 images canvasFG gameRef
            Just "ExecuteRound" -> do
                -- update GameState
                liftIO $ modifyIORef gameRef $ Model.setGameState GS_ExecuteRound

                --damage calculation ahs to happen before the actual attack
                liftIO $ modifyIORef gameRef (Model.calculatePDamage activePlayer)
                liftIO $ modifyIORef gameRef Model.executeAttacks
                liftIO $ modifyIORef gameRef Model.resetPlayableCards
                -- update Round Counter
                liftIO $ modifyIORef gameRef Model.incrementRoundCounter

                rand7 <- getRandom

                -- check if one player has won
                newGame <- liftIO $ readIORef gameRef
                if Model.isGameFinished newGame
                    then do
                        -- update GameState
                        liftIO $ modifyIORef gameRef $ Model.setGameState GS_Finished
                        -- return to splash screen
                        drawSplashScreen canvasBG canvasFG images buttonRow gameRef True
                    else do
                        -- update activePlayer
                        liftIO $ modifyIORef gameRef Model.switchToNextPlayer
                        liftIO $ modifyIORef gameRef $ Model.setGameState GS_WaitingForPlayer

                        let currentPlayer = Model.getActivePlayer game
                        debugLog currentPlayer
                        liftIO $ modifyIORef gameRef  (Model.initPBench currentPlayer rand7)
                        liftIO $ modifyIORef gameRef  (Model.giveOutCards rand7 currentPlayer)

                        drawGame canvasFG images gameRef


            Just other -> debugLog $ "clickArea "++other++" was clicked but has no click handler!"

        return ()

    return ()

-- | reset the given Game with the given random seed
resetGamestate :: Int -> Game -> Game
resetGamestate randSeed g = do
    let range = 2147483647 :: Int
    let randList = randomRs(-range, range)(mkStdGen randSeed )
    applyFunctList [
        Model.initPBench PlayerA (randList!!0),
        Model.initPBench PlayerB (randList!!0),
        Model.giveOutCards (randList!!1) PlayerA,
        Model.giveOutCards (randList!!2) PlayerB
        ] g

-- | check if a click occurred in the given clickAreas based on the given Point (for mouse coords) and Game (for context checks) and return the first-found hit as a Maybe
getHit :: UI.Point -> Game -> [Clickable] -> Maybe String
getHit p g clickAreas = gh where
    gh = if null hitAreas then Nothing else Just $ cId $ head hitAreas
    hitAreas = filter (and . sequence conditions) clickAreas
    conditions = [hovering p . cRect, elem (Model.getGameState g) . cGs, elem (Model.getActivePlayer g) . cAp]
    cId (x,_,_,_) = x
    cRect (_,x,_,_) = x
    cGs (_,_,x,_) = x
    cAp (_,_,_,x) = x

-- | Start a new Game
startGame :: UI.Canvas -> UI.Canvas -> ImgMap -> IORef Game -> UI ()
startGame canvasBG canvasFG images gameRef = do
    -- reset game
    liftIO $ modifyIORef gameRef Model.resetGame

    -- reset random-dependent fields
    rand <- getRandom
    liftIO $ modifyIORef gameRef (resetGamestate rand)
    
    -- update GameState
    liftIO $ modifyIORef gameRef Model.startGame

    -- clear canvases
    canvasBG # UI.clearCanvas
    canvasFG # UI.clearCanvas

    -- draw static game background
    drawGameBackground canvasBG images

    -- draw dynamic game foreground
    drawGame canvasFG images gameRef

    return ()

-- | schedule a waiting operation with the given String-ID, millisecond delay and gameRef.
scheduleWaitingOp :: String -> Int -> IORef Game -> UI ()
scheduleWaitingOp opID ms gameRef = do
    -- store the opID in the current Game
    liftIO $ modifyIORef gameRef $ Model.setWaitingOpID opID

    -- build and call a native JavaScript function scheduling an automated click on the #hiddenButton element after the given number of milliseconds. Yes, Threepenny also has a Timer module built-in, but it does not work too well outside the IO monad and seems to be less reliable.
    runFunction $ ffi "setTimeout(()=>{document.getElementById('hiddenButton').click();}, %1);" ms

-- | draw a primitive loading screen without images on the given canvas
drawLoadingScreen :: UI.Canvas -> UI ()
drawLoadingScreen canvasBG = do
    element canvasBG
        # set UI.textFont    "30px sans-serif"
        # set UI.fillStyle   (UI.htmlColor "#036d71")
        # set UI2.textBaseline UI2.Middle
    canvasBG # UI.fillText "Lade..." (640,275)
    element canvasBG # set UI2.textBaseline UI2.Alphabetic
    return ()

-- | draw the static game background on the given canvas
drawGameBackground :: UI.Canvas -> ImgMap -> UI ()
drawGameBackground canvasBG images = do
    -- background image
    bgImg <- getImg "bg" images
    canvasBG # UI.drawImage bgImg (referenceWidth*0.21,0)

    -- player one cards title
    cardsTitleImgOne <- getImg "titlePlayerOne" images
    canvasBG # UI.drawImage cardsTitleImgOne (playerACardsOffsetX,playerCardsOffsetY)

    -- player two title
    cardsTitleImgTwo <- getImg "titlePlayerTwo" images
    canvasBG # UI.drawImage cardsTitleImgTwo (playerBCardsOffsetX,playerCardsOffsetY)

    -- draw main board
    canvasBG # drawChessboard chessboardPos True (fromIntegral (Model.fieldRows -1)) (fromIntegral (Model.fieldColumns -1)) squareWidth ["SaddleBrown", "NavajoWhite"]

    -- draw player 1 bench
    canvasBG # drawChessboard player0benchPos True (fromIntegral (1 -1)) (fromIntegral (Model.fieldColumns -1)) squareWidth ["#ffe7a9", "#ffb700"]

    -- draw player 2 bench
    canvasBG # drawChessboard player1benchPos True (fromIntegral (1 -1)) (fromIntegral (Model.fieldColumns -1)) squareWidth ["#017a7f", "#03c9d1"]

    return ()

-- | load an image of the given MIME type and filename from the assets folder and return a pair of the given name and the <img> element
createImg :: String -> String -> String -> UI (String, Element)
createImg name mime filename = do
    url <- UI.loadFile mime $ "src/assets/" ++ filename
    img <- UI.img # set UI.src url
    return (name, img)

-- | shortcut for createImg with PNGs
createPNG :: String -> String -> UI (String, Element)
createPNG name filename = createImg name "image/png" filename

-- | fetch an image element by name/key from an 'ImgMap' and return in the 'UI' monad. If none is found, an error is logged and an empty image element is returned.
getImg :: String -> ImgMap -> UI Element
getImg key images = case lookup key images of
    Nothing -> do
        debugLog $ "Did not find image with key "++key
        UI.img
    Just img -> return img

-- | render a non-game splash screen on the given canvases
drawSplashScreen :: UI.Canvas -> UI.Canvas -> ImgMap -> Element -> IORef Game -> Bool -> UI ()
drawSplashScreen canvasBG canvasFG images buttonRow gameRef allowNewScore = do
    -- clear canvases
    canvasBG # UI.clearCanvas
    canvasFG # UI.clearCanvas

    -- get title image ready to draw
    titleImage  <- getImg "title" images

    -- set canvas text settings
    element canvasBG
        # set UI2.textBaseline UI2.Middle

    -- render invitation to start the game or a message that the game was won and can be restarted, based on the gameState
    game <- liftIO $ readIORef gameRef
    case Model.getGameState game of
        GS_NotStarted -> do
            canvasBG # UI.drawImage titleImage (490,183)
            element canvasBG
                # set UI.textFont      "19px sans-serif"
                # set UI.fillStyle     (UI.htmlColor "#03c9d1")
                # set UI2.textBaseline UI2.Middle
            canvasBG # UI2.fillRoundedRect (getPos playButtonRect) (playButtonWidth,playButtonHeight) 8
            element canvasBG # set UI.fillStyle (UI.htmlColor "white")
            canvasBG # UI.fillText "Spiel Starten" (getCenter playButtonRect)
            return ()
        GS_Finished -> do
            let winner = show $ Model.getActivePlayer game
            let rounds = Model.getRoundCounter game

            -- draw title image in the left
            canvasBG # UI.drawImage titleImage (265,183)

            -- draw highscores in the right
            scores <- getHighscores

            element canvasBG
                # set UI.textFont    "23px sans-serif"
                # set UI.fillStyle   (UI.htmlColor "#03b6be")
                # set UI.textAlign   UI.Start

            canvasBG # UI.fillText "Highscores:" (715, 210)

            element canvasBG
                # set UI.textFont    "17px serif"
                # set UI.fillStyle   (UI.htmlColor "black")

            canvasBG # drawHighscores scores (715, 240) 20

            -- draw winning message centered below
            element canvasBG
                # set UI.textFont    "23px sans-serif"
                # set UI.fillStyle   (UI.htmlColor "#03b6be")
                # set UI.textAlign   UI.Center

            canvasBG # UI.fillText (winner ++ " hat das Spiel in " ++ show rounds ++ " Runden gewonnen!") (640,440)

            -- add invitation to be added to highscores
            when (allowNewScore && isNewHighscore scores rounds) $ do
                element buttonRow # set UI.class_ ""
                canvasBG # UI.fillText "Du hast einen neuen Highscore erreicht und kannst Dich unten eintragen." (640,470)

            -- draw play-again button
            element canvasBG
                # set UI.textFont      "20px sans-serif"
                # set UI.fillStyle     (UI.htmlColor "#03c9d1")
                # set UI2.textBaseline UI2.Middle

            canvasBG # UI2.fillRoundedRect (getPos playAgainButtonRect) (getSize playAgainButtonRect) 8
            element canvasBG # set UI.fillStyle (UI.htmlColor "white")
            canvasBG # UI.fillText "Revanche?" (getCenter playAgainButtonRect)

            return ()
        _ -> return ()

    element canvasBG
        # set UI2.textBaseline UI2.Alphabetic
    return ()

-- | render the given highscores in the given position on the given canvas with the given number of pixels between the lines. If less highscores than highScoreLimit are given, they are rendered padded with "N/A"s.
drawHighscores :: Highscores -> UI.Point -> Double -> UI.Canvas -> UI ()
drawHighscores scores (xPos,yPos) gap canvas = dh scores (xPos,yPos) 1 where
    dh []     (xPos,yPos) c = when (c <= highScoreLimit) $ do
        canvas # UI.fillText (buildText ("N/A","N/A") c) (xPos,yPos)
        dh [] (xPos,yPos+gap) (c+1)
    dh (s:ss) (xPos,yPos) c = do
        canvas # UI.fillText (buildText s c) (xPos,yPos)
        dh ss (xPos,yPos+gap) (c+1)
    buildText s c = show c ++ ".  " ++ fst s ++ " (" ++ show (snd s) ++ ")"

-- | render the current game foreground on the given canvas
drawGame :: UI.Canvas -> ImgMap -> IORef Game -> UI ()
drawGame canvasFG images gameRef = do
    -- clear foreground
    canvasFG # UI.clearCanvas
    game <- liftIO $ readIORef gameRef

    let playerACards = Model.getPCards PlayerA game
    let playerBCards = Model.getPCards PlayerB game

    -- font styling
    element canvasFG
        # set UI.textFont    "14px sans-serif"
        # set UI.fillStyle   (UI.htmlColor "black")

    -- draw player card lists
    drawCardList playerACards playerACenterX globalplayerCardsOffsetY playerCardsStepY canvasFG
    drawCardList playerBCards playerBCenterX globalplayerCardsOffsetY playerCardsStepY canvasFG

    -- Always draw first (not Card_None) card in list as preview
    drawPreviewImage playerACards PlayerA (Model.getFirstValidCardPositionInList 0 PlayerA game) images canvasFG gameRef
    drawPreviewImage playerBCards PlayerB (Model.getFirstValidCardPositionInList 0 PlayerB game) images canvasFG gameRef

    -- draw player health info and left cards to play
    drawPlayerHealth (Model.getPHealth PlayerA game) (Model.getPHealth PlayerB game) canvasFG

    -- draw units on field and benches
    let p0Bench = Model.getPBench PlayerA game
    let p1Bench = Model.getPBench PlayerB game
    let field = Model.getField game

    -- active-player-dependent stuff
    let activePlayer = Model.getActivePlayer game
    drawUnits chessboardPos   activePlayer field canvasFG
    drawUnits player0benchPos activePlayer [p0Bench] canvasFG
    drawUnits player1benchPos activePlayer [p1Bench] canvasFG
    canvasFG # drawPlayerCardsLeft activePlayer (Model.getPlayableCardsLeft game)

    -- play card buttons
    playLeftImg <- getImg "playleft" images
    playRightImg <- getImg "playright" images
    -- end round buttons
    execRoundImgA <- getImg "endRoundP1" images
    execRoundImgB <- getImg "endRoundP2" images

    -- draw relevant buttons
    case activePlayer of
        PlayerA -> do
            canvasFG # UI.drawImage playLeftImg (getPos playCardARect)
            canvasFG # UI.drawImage execRoundImgA (getPos execButtonRect)
        PlayerB -> do
            canvasFG # UI.drawImage playRightImg (getPos playCardBRect)
            canvasFG # UI.drawImage execRoundImgB (getPos execButtonRect)

    return ()

-- | draw the given health info for both players on the given canvas
drawPlayerHealth :: Int -> Int -> UI.Canvas -> UI ()
drawPlayerHealth playerAHealth playerBHealth canvasFG = do
    element canvasFG
        # set UI.fillStyle   (UI.htmlColor "Silver")
        # set UI.textFont    "20px sans-serif"
    canvasFG # UI.fillText ("Leben: " ++ show playerAHealth) (playerACenterX,650)
    canvasFG # UI.fillText ("Leben: " ++ show playerBHealth) (playerBCenterX,650)

-- | draws card infos
drawPlayerCardsLeft :: PlayerID -> Int -> UI.Canvas -> UI()
drawPlayerCardsLeft pId am canvasFG = do
    element canvasFG
        # set UI.fillStyle   (UI.htmlColor (col am))
        # set UI.textFont    "17px sans-serif"
    if pId == PlayerA then do
        canvasFG # UI.fillText (show am ++ " Karte(n) zu spielen") (playerACenterX,  (334.0 + ((fromIntegral Model.cardAmount) * playerCardsStepY )))
    else do
        canvasFG # UI.fillText (show am ++ " Karte(n) zu spielen") (playerBCenterX,  (334.0 + ((fromIntegral Model.cardAmount) * playerCardsStepY )))
            where col x | x >= 3 = "#058100"
                        | x == 2 = "#FF9200" 
                        | x == 1 = "#FF9200"
                        | otherwise = "#A81100"

-- | recursively draw the given card list from the given X and Y offsets downwards with the given pixel step size on the given canvas
drawCardList :: [Card] -> Double -> Double -> Double -> UI.Canvas -> UI ()
drawCardList cards xOffset yOffset yStep canvasFG = dcl cards yOffset where
    dcl []     _       = return ()
    dcl (c:cs) yOffset = do
        canvasFG # UI.fillText (show (Model.getCardId c)) (xOffset,yOffset)
        dcl cs (yOffset + yStep)

-- | draws the preview images for a certain PlayerID
drawPreviewImage :: [Card] -> PlayerID  -> Int -> ImgMap -> UI.Canvas -> IORef Game -> UI()
drawPreviewImage cards playerID cardNr images canvasFG gameRef = do
    let activeCard = Model.setCardPosition cardNr (cards!!cardNr) -- set card position in the same time
    let pos = if playerID == PlayerA then getPos cardPreviewLeftRect else getPos cardPreviewRightRect
    let infoOffset = Model.getCardInfoOffset activeCard
    let varInfo    = Model.getCardVariation activeCard
    liftIO $ modifyIORef gameRef (Model.setActivePCard playerID activeCard)
    debugLog(show varInfo)
    cardImg <- getImg (show (Model.getCardId activeCard)) images
    canvasFG # UI.drawImage cardImg pos
    canvasFG # drawExtraInfo pos varInfo infoOffset

-- | draws additional info onto cards where needed
drawExtraInfo ::  UI.Point -> ([Int],[Int]) -> (UI.Point ,UI.Point ) -> UI.Canvas -> UI ()
drawExtraInfo pos ([],_) _                 canvas = return() -- hack to return unchanged canvas
drawExtraInfo pos (a,b) (aOffset, bOffset) canvas = do
    debugLog ("drawingExtraInfo")
    element canvas # set UI.fillStyle   (UI.htmlColor "black")
                   # set UI.textFont    "14px sans-serif"
    canvas # UI.fillText (show a) (addPoints pos  aOffset)
    canvas # drawExtraInfo pos (b,[]) (bOffset, (0,0))


-- | return the player-specific CSS color code for a given 'PlayerID'
playerColor :: PlayerID -> String
playerColor = pc where
    pc PlayerA = "darkred"
    pc PlayerB = "darkblue"

-- | draw all units on the given 'Field' to the given canvas
--   current player -> fieldposition -> Field to draw -> canvas
drawUnits :: UI.Point -> PlayerID -> Field -> UI.Canvas -> UI ()
drawUnits boardPos pId units canvas = drawUnits' boardPos pId (reverse units) canvas where
    drawUnits' boardPos pId ([])        canvas = return ()
    drawUnits' boardPos pId ([]:vs)     canvas = do
        drawUnits' boardPos pId vs canvas
        return ()
    drawUnits' boardPos pId ((u:us):vs) canvas = do
        case u of
            Just unit -> do
                drawFigure boardPos pId (fromIntegral(length vs)) (fromIntegral(length us))  squareWidth (Model.getUnitStrength unit) (Model.getUnitPlayer unit) canvas
            Nothing -> return ()
        drawUnits' boardPos pId (us:vs) canvas

-- | draw a unit figure with the given unit strength and position data on the given canvas
--   fieldPosition -> activePlayer -> pos1 -> pos2 -> width -> unitNum -> uPlayer > canvas
drawFigure ::  UI.Point -> PlayerID -> Double -> Double -> Double ->   Int -> PlayerID  -> UI.Canvas -> UI ()
drawFigure  boardpos  activePlayer xNum      yNum      squareWidth unit   player canvas     = do

    element canvas
                # set UI.textFont    "20px sans-serif"
                # set UI2.textBaseline UI2.Middle

    let absPosition = getSquarePosition boardpos squareWidth xNum yNum
    let centerpoint = getSquareCenter absPosition squareWidth

    case unit of

        -1->do
           element canvas # set UI.fillStyle   (UI.htmlColor "red")
           canvas # UI.fillText ("X") (fst centerpoint -3, snd centerpoint -5)
           canvas # UI.fillText ("X") (fst centerpoint +5, snd centerpoint +3)
           canvas # UI.fillText ("X") (fst centerpoint -2, snd centerpoint +2)

        0 -> do
            canvas # drawCircle centerpoint (playerColor player) False
            element canvas # set UI.fillStyle   (UI.htmlColor "Gray")
            canvas # UI.fillText (show unit) centerpoint
            return()
        1 -> do
            canvas # drawCircle centerpoint (playerColor player) True
            canvas # drawAttackIndicator centerpoint player activePlayer
            element canvas # set UI.fillStyle   (UI.htmlColor "Silver")
            canvas # UI.fillText (show unit) centerpoint
            return()
        2 -> do
            canvas # drawCircle centerpoint (playerColor player) True
            canvas # drawAttackIndicator centerpoint player activePlayer
            element canvas # set UI.fillStyle   (UI.htmlColor "Silver")
            canvas # UI.fillText (show unit) centerpoint
            return()
        3 -> do
            canvas # drawCircle centerpoint (playerColor player) True
            canvas # drawAttackIndicator centerpoint player activePlayer
            element canvas # set UI.fillStyle   (UI.htmlColor "DarkGray")
            canvas # UI.fillText (show unit) centerpoint
            return()
        4 -> do
            canvas # drawCircle centerpoint (playerColor player) True
            canvas # drawAttackIndicator centerpoint player activePlayer
            element canvas # set UI.fillStyle   (UI.htmlColor "DarkGray")
            canvas # UI.fillText (show unit) centerpoint
            return()
        5 -> do
            canvas # drawCircle centerpoint (playerColor player) True
            canvas # drawAttackIndicator centerpoint player activePlayer
            element canvas # set UI.fillStyle   (UI.htmlColor "DarkGray")
            canvas # UI.fillText (show unit) centerpoint
            return()
        6 -> do
            canvas # drawCircle centerpoint (playerColor player) True
            canvas # drawAttackIndicator centerpoint player activePlayer
            element canvas # set UI.fillStyle   (UI.htmlColor "DarkGray")
            canvas # UI.fillText (show unit) centerpoint
            return()
        7 -> do
            canvas # drawCircle centerpoint (playerColor player) True
            canvas # drawAttackIndicator centerpoint player activePlayer
            element canvas # set UI.fillStyle   (UI.htmlColor "DarkGray")
            canvas # UI.fillText (show unit) centerpoint
            return()
        8 -> do
            canvas # drawCircle centerpoint (playerColor player) True
            canvas # drawAttackIndicator centerpoint player activePlayer
            element canvas # set UI.fillStyle   (UI.htmlColor "DarkGray")
            canvas # UI.fillText (show unit) centerpoint
            return()
        9 -> do
            canvas # drawCircle centerpoint (playerColor player) True
            canvas # drawAttackIndicator centerpoint player activePlayer
            element canvas # set UI.fillStyle   (UI.htmlColor "DarkSlateGray")
            canvas # UI.fillText (show unit) centerpoint
            return()
        10 -> do
            canvas # drawCircle centerpoint (playerColor player) True
            canvas # drawAttackIndicator centerpoint player activePlayer
            element canvas # set UI.fillStyle   (UI.htmlColor "DarkSlateGray")
            canvas # UI.fillText (show unit) centerpoint
            return()
        _ -> return()

    element canvas
        # set UI2.textBaseline UI2.Alphabetic
    return()

-- | draws little triangle onto the units for directional infos
drawAttackIndicator :: UI.Point -> PlayerID -> PlayerID -> UI.Canvas -> UI()
drawAttackIndicator pos PlayerA PlayerA canvas = drawTriangle pos "white" (-1.0) canvas
drawAttackIndicator pos PlayerB PlayerB canvas = drawTriangle pos "white"  (1.0) canvas
drawAttackIndicator _ _ _ _                    = return ()

-- | triangle helper function
drawTriangle :: UI.Point -> String -> Double -> UI.Canvas -> UI()
drawTriangle   (xPos, yPos) htmlColor   stretch  canvas     = do
    element canvas # set UI.fillStyle (UI.htmlColor htmlColor)
    canvas # UI.beginPath
    canvas # UI.moveTo (xPos - 20*stretch, yPos)
    canvas # UI.lineTo (xPos - 10*stretch, yPos+10)
    canvas # UI.lineTo (xPos - 10*stretch, yPos-10)
    canvas # UI.closePath
    canvas # UI.fill

-- | draw a circle with the given position and color on the given canvas
drawCircle :: UI.Point      -> String     -> Bool -> UI.Canvas  -> UI()
drawCircle   circleCenterpoint htmlColor   fill  canvas     = do

    let radius = squareWidth / 3

    canvas # UI.beginPath
    canvas # UI.arc circleCenterpoint radius 0 (2*pi)
    canvas # UI.closePath

    if fill then do
        element canvas # set UI.fillStyle (UI.htmlColor htmlColor)
        canvas # UI.fill
    else do
        element canvas # set UI.strokeStyle htmlColor
        canvas # UI.stroke

    return ()


-- | Draw the chessboard background to the canvas. 
drawChessboard :: (Double, Double) -> Bool -> Double -> Double -> Double -> [String] -> UI.Canvas -> UI ()
drawChessboard (xPos,yPos) createRow xSquare ySquare squareWidth colors canvas = dc createRow xSquare ySquare colors where
    -- traverse y downwards, draw first square depending on gap, init drawing of rest of vertical row : end condition y = -1
    dc True  xNum (-1) (c:cs) = return ()

    -- traverse row : end condition x = -1
    dc False (-1) yNum (c:cs) = return ()

    -- traverse y downwards, draw first square of each row and start drawing of row
    dc True  xNum yNum (c:cs) = do
        element canvas # set UI.fillStyle (UI.htmlColor c)
        canvas # UI.fillRect ( xNum * squareWidth + xPos , yNum * squareWidth + yPos) squareWidth squareWidth
        canvas # drawChessboard (xPos,yPos) True   xNum    (yNum -1) squareWidth (cs ++ [c])
        canvas # drawChessboard (xPos,yPos) False (xNum -1) yNum     squareWidth (cs ++ [c])
        return ()

    -- traverse row
    dc False xNum yNum (c:cs) = do
        element canvas # set UI.fillStyle (UI.htmlColor c)
        canvas # UI.fillRect (  xNum * squareWidth + xPos , yNum * squareWidth + yPos) squareWidth squareWidth
        canvas # drawChessboard (xPos,yPos) False (xNum -1) yNum squareWidth (cs ++ [c])
        return ()

    -- exhaustive case
    dc _ _ _ _ = do
        debugLog "Unexpected case in drawChessboard call!"
        return ()

-- | get the current top highscores as Highscores
getHighscores :: UI Highscores
getHighscores = do
    scoresRaw <- liftIO readHighscoresCSV
    debugLog $ "Read Previous Highscores: "++show scoresRaw --this will also avoid any errors around not-fully consumed contents of the file before writing back to it
    return $ topScores highScoreLimit $ highscoresFromCSV scoresRaw

-- | read the current highscores as CSV: Try to read from the scores.csv file, if none exists return the defaultHighscores instead.
readHighscoresCSV :: IO CSV
readHighscoresCSV = do
    fileContent <- (try::IO String -> IO (Either IOException String)) $ readFile "src/scores.csv"
    case fileContent of
        Left err -> return defaultHighscores
        Right content -> return $ fromCSV content

writeHighscores :: Highscores -> UI ()
writeHighscores scores = do
    liftIO $ writeHighscoresCSV $ highscoresToCSV scores

-- | write the given highscores as CSV to the scores.csv file
writeHighscoresCSV :: CSV -> IO ()
writeHighscoresCSV newScores = do
    let csvString = toCSV newScores
    writeFile "src/scores.csv" csvString

-- | default highscores in CSV format
defaultHighscores :: CSV
defaultHighscores = [csv|
    Chuck Norris, 1
    Mr. Bean,2000
    Normaler Dude, 30
    Jan,50
    |]

-- | get the high scores as tuples from the given CSV. Requires two values per row in the CSV, the second of which has to be parsable to a number.
highscoresFromCSV :: CSV -> Highscores
highscoresFromCSV (CSV csv) = fmap extract csv where
    extract line = (name line, score line)
    name line = line!!0
    score line = read (line!!1)::Int

-- | get the CSV back-translation of the given highscores
highscoresToCSV :: Highscores -> CSV
highscoresToCSV scores = CSV $ fmap extract scores where
    extract s = [fst s,show $ snd s]

-- | sort the given scores and take the top (lowest) 'limit' ones.
topScores :: Int -> Highscores -> Highscores
topScores limit scores = take limit ordList where
    ordList = sortBy srt scores
    srt a b = compare (snd a) (snd b)

-- | return whether or not the given newScore is worth a new highscore entry, based on whether it is lower (less rounds = better) than the last of the given, already ordered scores list or if there are empty places left to fill based on the highscoreLimit
isNewHighscore :: Highscores -> Int -> Bool
isNewHighscore scores newScore = length scores < highScoreLimit || newScore < snd (last scores)

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Data.List (find)
import MapData (initialMap, Tile, Map)  
import Data.Maybe (isJust, fromJust)

data Player = Player
  { playerPos :: (Float, Float)
  , playerVel :: (Float, Float)
  }

type Dialogue = Maybe String

type GameMap = [[Int]]


data Quest = Quest
  { questName :: String
  , questDescription :: String
  , questCompleted :: Bool
  } deriving (Eq)

data NPC = NPC
  { npcPos :: (Float, Float)
  , dialogues :: [String]
  , currentDialogueIndex :: Int
  , name :: String
  , npcQuest :: Maybe Quest
  } deriving (Eq)

data GameStatus = Loading | Running

data GameState = GameState
  { gameMap :: GameMap
  , player :: Player
  , time :: Float
  , npcs :: [NPC]
  , currentDialogue :: Dialogue
  , dialogueTimer :: Float
  , quests :: [Quest]
  , status :: GameStatus
  }

main :: IO ()
main = play window background fps initialState render handleEvents update

window :: Display
window = InWindow "Haskell Game!" (800, 600) (100, 100)

background :: Color
background = greyN 0.7

fps :: Int
fps = 60

tileSize :: Float
tileSize = 20

tileSpacing :: Float
tileSpacing = 0

windowWidth :: Float
windowWidth = 800

windowHeight :: Float
windowHeight = 600

initialPlayer :: Player
initialPlayer = Player { playerPos = (500, -200), playerVel = (0, 0) }

initialNpcs :: [NPC]
initialNpcs = [NPC { npcPos = (55, 25)
                   , dialogues = [ "Welcome Adventurer!"
                                 , "Where did you come from?"
                                 , "Hoyteknologisenteret? I haven't\nheard of that place in a long time."
                                 , "Anyway, I seem to have lost my\ncharger, could you please help me find it?"
                                 ]
                   , currentDialogueIndex = 0
                   , name = "Professor X"
                   , npcQuest = Just (Quest "Find Charger" "Retrieve the lost charger from the nearby woods." False)
                   }]

moreNpcs :: [NPC]
moreNpcs =
  [ NPC { npcPos = (1100, -1175)
        , dialogues = ["Hello, young explorer.", "Beware of the dangers that lie ahead.", "Take care."]
        , currentDialogueIndex = 0
        , name = "King Haakon"
        , npcQuest = Nothing
        }
  , NPC { npcPos = (0, -1500)
        , dialogues = ["Do you need any armour crafted?", "I should be able to soon."]
        , currentDialogueIndex = 0
        , name = "Blacksmith"
        , npcQuest = Nothing
        }
  ]

questItemNPC :: NPC
questItemNPC = NPC { npcPos = (1100, -1700) 
                   , dialogues = ["You found the charger! Return it to Professor X."]
                   , currentDialogueIndex = 0
                   , name = "Lost Charger"
                   , npcQuest = Just (Quest "Find Charger" "(Complete!) Return to Professor X to recieve your award!" False) 
                   }

initialState :: GameState
initialState = GameState
  { gameMap = initialMap
  , player = initialPlayer
  , time = 0
  , npcs = initialNpcs ++ moreNpcs  
  , currentDialogue = Nothing
  , dialogueTimer = 0
  , status = Loading
  , quests = []  
  }


beige :: Color
beige = makeColorI 245 245 220 255  

wood :: Color
wood = makeColorI 139 69 19 255  

darkerGreen :: Color
darkerGreen = makeColorI 0 100 0 100  

myOrange :: Color
myOrange = makeColorI 255 165 0 255  

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

renderTile :: Tile -> Picture
renderTile 0 = Color green $ rectangleSolid (tileSize + 2) (tileSize + 2)  -- Grass
renderTile 1 = Color black $ rectangleSolid (tileSize + 2) (tileSize + 2)  -- House
renderTile 2 = Color (greyN 0.7) $ rectangleSolid (tileSize + 2) (tileSize + 2)  -- Door
renderTile 3 = Color beige $ rectangleSolid (tileSize + 2) (tileSize + 2)  -- Path
renderTile 4 = Color wood $ rectangleSolid (tileSize + 2) (tileSize + 2)  -- Wood
renderTile 5 = Color blue $ rectangleSolid (tileSize + 2) (tileSize + 2)  -- Water
renderTile 6 = Color (greyN 0.5) $ rectangleSolid (tileSize + 2) (tileSize + 2)  -- Border
renderTile 7 = Color darkerGreen $ rectangleSolid (tileSize + 2) (tileSize + 2)  -- Darker Grass
renderTile 8 = Color myOrange $ rectangleSolid (tileSize + 2) (tileSize + 2)  -- Orange for Blacksmith

renderMap :: Map -> Picture
renderMap tiles = Pictures [translate x y $ renderTile tile | (yRow, row) <- zip [0..] tiles, (xCol, tile) <- zip [0..] row, let x = fromIntegral xCol * 22 - 100, let y = fromIntegral yRow * (-22) + 100]

render :: GameState -> Picture
render gameState = case status gameState of
  Loading -> loadingScreen
  Running -> gameScreen gameState
  where
    loadingScreen = Color white $ Translate (-100) 0 $ Scale 0.3 0.3 $ Text "Loading..."

    gameScreen gs =
      let
        (px, py) = playerPos $ player gs

        boxWidth = 700
        boxHeight = 100
        borderSize = 10 

        boxX = windowWidth / 2 - boxWidth / 2 - borderSize - 40
        boxY = borderSize - 200  

        dialogueBox = case currentDialogue gs of
            Just txt -> pictures
                          [ translate boxX boxY $ color black $ rectangleSolid (boxWidth + 2 * borderSize) (boxHeight + 2 * borderSize)
                          , translate boxX boxY $ color white $ rectangleSolid boxWidth boxHeight
                          , translate (boxX - 300) (boxY + 15) $ color black $ scale 0.2 0.2 $ renderText txt
                          ]
            Nothing -> Blank

        renderText :: String -> Picture
        renderText text = Pictures $ zipWith (\n line -> Translate 0 (- (175 * fromIntegral n)) $ Text line) [0..] (lines text)

        questBoxWidth = 700
        questBoxHeight = 100
        questBoxX = 0
        questBoxY = 225
        questDisplay = renderQuests (quests gs)
        questBox = if not (null (quests gs)) then
                      pictures [
                        translate questBoxX questBoxY $ color yellow $ rectangleSolid (questBoxWidth + borderSize) (questBoxHeight + borderSize),
                        translate questBoxX questBoxY $ color white $ rectangleSolid questBoxWidth questBoxHeight,
                        translate (-325) 225 $ color black $ scale 0.15 0.15 questDisplay
                      ]
                   else Blank

      in pictures
        [ translate (-px) (-py) $ pictures
            [ renderMap (gameMap gs)
            , renderPlayer (player gs)
            , pictures $ map renderNPC (npcs gs)
            ]
        , dialogueBox
        , questBox  
        ]

renderQuests :: [Quest] -> Picture
renderQuests quests = Pictures $ zipWith (\n quest -> Translate 0 (- (30 * fromIntegral n)) $ Text (questName quest ++ if questCompleted quest then " (Completed)" else ": " ++ questDescription quest)) [0..] quests

renderNPC :: NPC -> Picture
renderNPC npc =
  let (x, y) = npcPos npc
      npcIcon = translate x y $ color red $ circleSolid 10
      npcName = translate (x - 10) (y + 20) $ scale 0.1 0.1 $ color white $ text (name npc)
  in pictures [npcIcon, npcName]

renderPlayer :: Player -> Picture
renderPlayer player =
  let (x, y) = playerPos player
  in translate x y $ color magenta $ circleSolid 10

processDialogue :: GameState -> String
processDialogue gameState =
  let (px, py) = playerPos $ player gameState
      npcInRange = find (\npc -> distance (npcPos npc) (px, py) < 20) (npcs gameState)
  in case npcInRange of
       Just npc -> if currentDialogueIndex npc < length (dialogues npc)
                   then dialogues npc !! currentDialogueIndex npc
                   else "No more dialogues"
       Nothing -> "Press 'E' to talk"






moveAmount :: Float
moveAmount = 150  

canMoveTo :: (Float, Float) -> Map -> (Bool, String)
canMoveTo (x, y) gameMap =
    let mapWidth = maximum (map length gameMap) 
        mapHeight = length gameMap
        playerRadius = 10
        leftX = max 0 (floor (x - playerRadius + 100) `div` 22)
        rightX = min (mapWidth - 1) (floor (x + playerRadius + 100) `div` 22)
        topY = max 0 (floor (-y - playerRadius + 100) `div` 22)
        bottomY = min (mapHeight - 1) (floor (-y + playerRadius + 100) `div` 22)
        tiles = [gameMap !! ty !! tx |
                   ty <- [topY .. bottomY],
                   ty >= 0,
                   ty < mapHeight,
                   tx <- [leftX .. rightX],
                   tx >= 0,
                   tx < mapWidth]
        debugInfo = unlines [
            "Player position: " ++ show (x, y),
            "Tile indices checked: " ++ show [(ty, tx) | ty <- [topY..bottomY], tx <- [leftX..rightX]],
            "Tiles: " ++ show tiles ]
    in (all (\tile -> tile /= 1 && tile /= 5 && tile /= 6) tiles, debugInfo)

handleEvents :: Event -> GameState -> GameState
handleEvents event gameState =
  case event of
    (EventKey (SpecialKey KeyUp) Down _ _) ->
      gameState { player = (player gameState) { playerVel = (0, moveAmount) } }
    (EventKey (SpecialKey KeyDown) Down _ _) ->
      gameState { player = (player gameState) { playerVel = (0, -moveAmount) } }
    (EventKey (SpecialKey KeyLeft) Down _ _) ->
      gameState { player = (player gameState) { playerVel = (-moveAmount, 0) } }
    (EventKey (SpecialKey KeyRight) Down _ _) ->
      gameState { player = (player gameState) { playerVel = (moveAmount, 0) } }
    (EventKey key Up _ _) ->
      gameState { player = (player gameState) { playerVel = (0, 0) } }  
    (EventKey (Char 'e') Down _ _) -> interactWithNPC gameState
    _ -> gameState
  where
    interactWithNPC gs = maybe gs (handleNPC gs) (find (npcInRange (playerPos $ player gs)) (npcs gs))
    handleNPC gs npc = if currentDialogueIndex npc < length (dialogues npc)
                       then advanceDialogue npc gs
                       else addQuest npc gs
    npcInRange (px, py) npc = distance (npcPos npc) (px, py) < 20
    advanceDialogue npc gs = gs {
        npcs = map (updateNPCDialogue npc) (npcs gs),
        currentDialogue = Just (dialogues npc !! currentDialogueIndex npc),
        dialogueTimer = 5.0 
    }
    updateNPCDialogue npc n = if npc == n
                              then n { currentDialogueIndex = currentDialogueIndex n + 1 }
                              else n
    addQuest npc gs = case npcQuest npc of
      Just quest -> if not (questCompleted quest) && quest `notElem` quests gs
                    then
                      let newQuests = quest : quests gs
                          newNpcs = if questName quest == "Find Charger"  
                                    then questItemNPC : npcs gs  
                                    else npcs gs
                      in gs { quests = newQuests, npcs = newNpcs, currentDialogue = Nothing }  
                    else gs
      Nothing -> gs { currentDialogue = Nothing } 


update :: Float -> GameState -> GameState
update dt gameState = case status gameState of
  Loading -> if time gameState > 3
             then gameState { status = Running, time = 0 }
             else gameState { time = time gameState + dt }
  Running -> updateGameState dt gameState

updateGameState :: Float -> GameState -> GameState
updateGameState dt gameState =
  let p = player gameState
      (px, py) = playerPos p
      (vx, vy) = playerVel p
      newPos = (px + vx * dt, py + vy * dt)
      updatedDialogueTimer = dialogueTimer gameState - dt
      updatedDialogue = if updatedDialogueTimer <= 0 then Nothing else currentDialogue gameState
      (canMove, debugOutput) = canMoveTo newPos (gameMap gameState)
  in if canMove then
       gameState { player = p { playerPos = newPos }, dialogueTimer = updatedDialogueTimer, currentDialogue = updatedDialogue }
     else
       gameState { dialogueTimer = updatedDialogueTimer, currentDialogue = updatedDialogue } 

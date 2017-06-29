{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Brick
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Control.Monad (void)
import Data.Matrix
import qualified Data.Vector as Ve
import Data.List
import Data.List.Split
import Control.Monad.IO.Class

type Name = ()
data Cell = Lit | Unlit deriving (Eq)
data DirectionDir = UpDir | DownDir | LeftDir | RightDir
data Pos = Pos { _x :: Int, _y :: Int } deriving (Eq)
type Board = Matrix Cell
data Game = Game { _board :: Board, _pos :: Pos, _levels :: [Board], _level :: Int }

gameOver :: Game -> Bool
gameOver g@Game{..} = allClear _board && _level == 49

allClear :: Board -> Bool
allClear board = all (== Unlit) (toList board)

drawUI :: Game -> [Widget Name]
drawUI g = if gameOver g
              then [ C.center $ str "Good job! Press q to exit!" ]
              else [ C.center $ drawGrid g ]

drawGrid :: Game -> Widget Name
drawGrid (Game board position _ level) = vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [1..5]] ++ [hBox [write $ "Level " ++ show (level + 1)]]
    cellsInRow r = [cellAt r c | c <- [1..5]]
    cellAt r c = if position == Pos c r
                    then drawSelected $ getCell r c
                    else drawCell $ getCell r c
    getCell r c = unsafeGet r c board

drawCell :: Cell -> Widget Name
drawCell Lit = withAttr litAttr cw
drawCell Unlit = withAttr unlitAttr cw

drawSelected :: Cell -> Widget Name
drawSelected Lit = withAttr litAttr hcw
drawSelected Unlit = withAttr unlitAttr hcw

cw :: Widget Name
cw = str "  "

hcw :: Widget Name
hcw = str "[]"

write :: String -> Widget Name
write = withAttr fontAttr . str

litAttr, unlitAttr, fontAttr :: AttrName
litAttr = "litAttr"
unlitAttr = "unlitAttr"
fontAttr = "fontAttr"

applyAll :: [a -> a] -> a -> a
applyAll = foldr (.) id

toggleBoard :: Pos -> Board -> Board
toggleBoard pos = applyAll toggles
  where
    toggles = togglePosition <$> nub positions
    positions = pos:neighbors
    neighbors = flip go pos <$> [UpDir, DownDir, LeftDir, RightDir]

togglePosition :: Pos -> Board -> Board
togglePosition (Pos x y) board = unsafeSet cell' (y, x) board
  where
    cell' = toggleCell $ unsafeGet y x board

toggleCell :: Cell -> Cell
toggleCell Lit = Unlit
toggleCell Unlit = Lit

toggle :: Game -> Game
toggle g = if allClear (_board g') && _level g' < 49
              then g' { _board = nextBoard
                      , _pos = Pos 1 1
                      , _level = _level g' + 1
                      }
              else g'
  where
    g' = g {_board = toggleBoard (_pos g) (_board g)}
    nextBoard = _levels g !! (_level g' + 1)

move :: DirectionDir -> Game -> Game
move dir g@Game{_pos} = g {_pos = go dir _pos}

go :: DirectionDir -> Pos -> Pos
go UpDir (Pos x y) = Pos x (max 1 (y - 1))
go DownDir (Pos x y) = Pos x (min 5 (y + 1))
go RightDir (Pos x y) = Pos (min 5 (x + 1)) y
go LeftDir (Pos x y) = Pos (max 1 (x - 1)) y

handleEvent :: Game -> BrickEvent Name () -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = continue $ resetLevel g
handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ move UpDir g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ move DownDir g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue $ move RightDir g
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ move LeftDir g
handleEvent g (VtyEvent (V.EvKey V.KEnter [])) = continue $ toggle g
handleEvent g _ = continue g

resetLevel :: Game -> Game
resetLevel g@Game{_level, _levels} = g{ _board = _levels !! _level }

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (litAttr, V.black `on` V.red)
  , (unlitAttr, V.black `on` V.blue)
  , (fontAttr, V.white `on` V.black)
  ]

load :: String -> Board
load lvl = fromList 5 5 cells
  where
    cells = [toCell x | x <- lvl]
    toCell '.' = Unlit
    toCell 'X' = Lit
    toCell c = error $ "Invalid char: " ++ show c

emptyBoard :: Board
emptyBoard = matrix 5 5 (const Unlit)

lightsOut :: App Game () Name
lightsOut = App { appDraw = drawUI
                , appChooseCursor = neverShowCursor
                , appHandleEvent = handleEvent
                , appStartEvent = return
                , appAttrMap = const theMap
                }

loadLevels :: String -> IO [Board]
loadLevels levelFile = do
  levelData <- lines <$> readFile levelFile
  return $ map (load . concat) (chunksOf 5 levelData)

initGame :: IO Game
initGame = do
  levels <- loadLevels "levels.data"
  return $ Game (head levels) (Pos 1 1) levels 0

main :: IO ()
main = initGame >>= void . (defaultMain lightsOut)

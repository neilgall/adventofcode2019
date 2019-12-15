{-# LANGUAGE BlockArguments #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Debug.Trace (trace)
import Control.Exception (bracket)
import Control.Monad (foldM, when, unless)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import System.Console.ANSI (clearScreen, setCursorPosition)

-- Socket client

runClient :: (Socket -> IO a) -> IO a
runClient client = do 
    let hints = defaultHints { addrSocketType = Stream }
    addr <- fmap head $ getAddrInfo (Just hints) (Just "127.0.0.1") (Just "5000")
    bracket (open addr) close client
  where
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock (addrAddress addr)
      return sock

testClient :: Socket -> IO ()
testClient sock = do
  sendAll sock (B.cons '1' B.empty)
  msg <- recv sock 1
  putStrLn (show $ B.head msg)


-- High level description of socket protocol

data Move = North | South | East | West
  deriving (Eq, Show)

data Status = Blocked | Clear | Oxygen
  deriving (Eq, Show)

reverseMove :: Move -> Move
reverseMove North = South
reverseMove South = North
reverseMove East = West
reverseMove West = East

messageFromMove :: Move -> B.ByteString
messageFromMove move = B.cons (c move) B.empty
  where
    c North = '1'
    c South = '2'
    c West = '3'
    c East = '4'

statusFromMessage :: B.ByteString -> Status
statusFromMessage = status . B.head
  where
    status '0' = Blocked
    status '1' = Clear
    status '2' = Oxygen


-- Exploration model

data Position = Position Int Int
  deriving (Eq, Ord, Show)

data Cell = Unexplored | Empty | Wall | Target
  deriving (Eq, Show)

data BoundingBox = BoundingBox Position Position
  deriving (Show)

data RepairDroid = RepairDroid {
  area      :: M.Map Position Cell,
  position  :: Position,
  oxygenPos :: Maybe Position,
  pastMoves :: [Move]
}

instance Show RepairDroid where
  show droid = "pos=" ++ (show $ position droid)
            ++ " oxygen:" ++ (show $ oxygenPos droid)
            ++ " moves:" ++ (show $ length $ pastMoves droid)

scan :: RepairDroid -> (Position -> Cell -> Char) -> String
scan droid showCell = header ++ body
  where
    header = (show minx) ++ "," ++ (show miny)
          ++ " -> " ++ (show maxx) ++ "," ++ (show maxy)
          ++ "\n" ++ (show droid) ++ "\n\n"

    body = concat $ L.intersperse "\n" bodyLines

    bodyLines =  map bodyLine [miny..maxy]

    bodyLine y = map (cell y) [minx..maxx]

    cell y x =
        let
          pos = Position x y
          cell = fromMaybe Unexplored $ M.lookup pos (area droid)
        in
          showCell pos cell

    positions = [Position (-5) (-5), Position 5 5] ++ M.keys (area droid)
    minx = L.minimum (map x positions)
    maxx = L.maximum (map x positions)
    miny = L.minimum (map y positions)
    maxy = L.maximum (map y positions)
    x (Position x _) = x
    y (Position _ y) = y


visualise :: RepairDroid -> IO ()
visualise droid = do
  clearScreen
  setCursorPosition 0 0
  putStrLn $ scan droid showCell
  where
    showCell p c =
      if p == position droid
        then 'X'
        else case c of
          Wall -> '#'
          Empty -> '.'
          Target -> 'O'
          Unexplored -> ' '


makeDroid :: RepairDroid
makeDroid = RepairDroid { 
  area      = M.empty,
  position  = Position 0 0,
  oxygenPos = Nothing,
  pastMoves = []
}


applyMove :: Position -> Move -> Position
applyMove pos move = 
  let
    (Position x y) = pos
  in
    case move of
      North -> Position x (y-1)
      South -> Position x (y+1)
      West  -> Position (x-1) y
      East  -> Position (x+1) y


applyStatus :: RepairDroid -> Move -> Bool -> Status -> RepairDroid
applyStatus droid move backtrack status =
  let
    moveList = if backtrack then tail (pastMoves droid) else move:(pastMoves droid)
    targetPos = applyMove (position droid) move
    set x = M.insert targetPos x (area droid)
  in
    case status of
      Blocked -> 
        droid { area = set Wall }
      Clear ->
        droid { area = set Empty, position = targetPos, pastMoves = moveList }
      Oxygen ->
        droid { area = set Target, position = targetPos, oxygenPos = Just targetPos, pastMoves = moveList }


explore :: RepairDroid -> [Move]
explore droid = catMaybes [look d | d <- [North,East,South,West]]
  where
    pos = position droid
    look d = case M.lookup (applyMove pos d) (area droid) of
      Nothing -> Just d
      Just _ -> Nothing

data Action 
  = Stop Position
  | Moves [Move]
  | Backtrack Move
  | Failure
  deriving (Show)

backtrackMove :: RepairDroid -> Move
backtrackMove = reverseMove . head . pastMoves

nextAction :: RepairDroid -> Action
nextAction droid = 
  case oxygenPos droid of
    Just pos ->
      Stop pos
    Nothing -> 
      if null moves
        then if null (pastMoves droid)
          then Failure
          else Backtrack (backtrackMove droid)
        else Moves moves
  where
    moves = explore droid

firstResult :: [Maybe a] -> Maybe a
firstResult = listToMaybe . catMaybes

findOxygenClient :: Socket -> IO RepairDroid
findOxygenClient sock = run makeDroid
  where
    run droid = do
      -- visualise droid
      -- getLine
      case nextAction droid of
        Stop pos -> 
          return droid
        Moves moves -> do
          foldM doMoveIfNotFound droid moves
        Backtrack move ->
          doMove droid True move
        Failure ->
          return droid

    doMoveIfNotFound droid move' =
      case oxygenPos droid of
        Nothing -> doMove droid False move'
        Just _ -> return droid

    doMove droid backtrack move' = do
      sendAll sock (messageFromMove move')
      msg <- recv sock 1
      run $ applyStatus droid move' backtrack (statusFromMessage msg)

main = do
  r <- runClient findOxygenClient
  putStrLn $ "Part 1 .. " ++ (show $ length $ pastMoves r)

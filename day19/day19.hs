{-# LANGUAGE BlockArguments #-}

import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import qualified Data.Map as M
import Data.Char (chr, ord)
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

data Position = Position Int Int
  deriving (Eq, Show)

data Status = Pulled | Stationary
  deriving (Eq, Show)

messageFromPosition :: Position -> B.ByteString
messageFromPosition (Position x y) = B.pack [chr x, chr y]

statusFromMessage :: B.ByteString -> Status
statusFromMessage m = if B.head m == '\0' then Stationary else Pulled

queryLocation :: (Socket) -> Position -> IO Status
queryLocation sock pos = do
  sendAll sock (messageFromPosition pos)
  msg <- recv sock 1
  return $ statusFromMessage msg


part1 :: IO Int
part1 =
  foldM scanRow 0 [0..49]
  where
    scanRow total y = foldM (scanPos y) total [0..49]
    scanPos y total x = do
      status <- runClient $ flip queryLocation (Position x y)
      case status of
        Pulled -> return (total + 1)
        Stationary -> return total

main = do
  affectedPoints <- part1
  putStrLn $ "Part 1.. " ++ (show affectedPoints)

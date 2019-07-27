module Main where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Exception -- The control exception
import Control.Monad.Fix (fix) -- Define a monadic fix point(no ?)
import Control.Monad (when) -- when




type Msg = ( Int ,  String) --- defines as message and the compiler knows its a String

main :: IO()
main = do
    let hints = defaultHints { addrFlags=[AI_PASSIVE], addrSocketType=Stream}
    addr : _ <- getAddrInfo (Just hints) Nothing (Just "9140")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 2 -- maximum of two idle connections channels that come here have no new channels
    chan <- newChan -- No one is reading from this channel
    mainLoop sock chan 0

--- This is where the application and such applications need to come back for usage
--- This is what happens when such values need to come along here
--- Does this mean that the values that come here
--- let us run the main Loop-
---
mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
    conn <- accept sock
    forkIO (runConn conn chan msgNum) -- Run each connection on its own separate thread
    mainLoop sock chan $! msgNum + 1 -- Run this in the main loop of the application

-- This is where the values that come as the values come along
-- The values that are used here that comes along and the values
-- The response serves as an application that needs to come up along here
runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Hi, what's your name?"
    name <- fmap init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered chat.")
    hPutStrLn hdl ("Welcome, " ++ name ++ "!")
    commLine <- dupChan chan -- This is where we do our communication

    -- Make sure we not reading the same number over and over again
    -- So we do a simple check that makes sure the next num is there
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line ) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop

    --- This just makes some basic io work doable
    --- Talking/thinking about effects is what I want to do here
    handle (\(SomeException _) -> return ()) $ fix $ \loop ->do
        line <- fmap init (hGetLine hdl) -- get the line in the line and handle
        case line of
            "quit" -> hPutStrLn hdl "Bye!" -- type quity you leave and this is what the application comes
            _      -> broadcast (name ++ ": " ++ line) >> loop -- What does the loop function do

    -- The server is running but it understands http
    -- So how does the value comes here
    -- We have a bug though no One is doin some values as they comes
    killThread reader
    broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
    hClose hdl

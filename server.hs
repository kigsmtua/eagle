module Main where

import System.Posix
import Network
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Network.Socket (close)
import Control.Monad (forever, replicateM_)
import System.Environment (getArgs)
import System.IO

processRequest h = do
    s <- hGetContents h
    let req = takeWhile (/= "\r") $ lines s
    mapM_ (\x -> putStrLn $ "> "++x) req
    return $ words (head req) !! 1

responseHtml h file= hPutStr h $
        "HTTP/1.1 200 OK\n" ++
        "Content-Type: text/html\n" ++
        "\n" ++
        "<html>\n" ++
        "<body>\n" ++
        "<h1><a>File to download:</h1>\n" ++
        "<a href=\""++file++"\">"++file++"</a>\n" ++
        "</body>\n" ++
        "</html>"

responseFile file mediatype h = do
    status <- getFileStatus file
    let size = show $ fileSize status
    hPutStr h $
            "HTTP/1.0 200 OK\n"++
            "Content-Type: "++mediatype++"\n"++
            "Content-Length: "++size++"\n"++
            "\n"
    let handler fileHandle = do
            hSetBinaryMode fileHandle True
            contents <- B.hGetContents fileHandle
            B.hPut h contents
    withFile file ReadMode handler

processRequests socket file = do
    putStrLn "accepting sockets."
    (h, hostname, port) <- accept socket
    putStrLn $ "socket accepted: " ++ show (hostname, port)
    req <- processRequest h
    if req == "/" then do
            responseHtml h file
            putStr  "Page served... waiting\n"
            hClose h
            processRequests socket file
        else do
            responseFile file "application/octet-stream" h
            putStr  "File transferred\n closing handle\n"
            hClose h

main = do
    xs <- getArgs
    case xs of
        port:filename:_  -> do
            installHandler sigPIPE Ignore Nothing;--don't crash on connection ending
            socket <- listenOn (PortNumber (fromIntegral $ read port))
            processRequests socket filename
            putStrLn "Closing socket"
            close socket
        _ -> putStrLn "need a port and filename"

import Network
import System.IO
import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM
import Data.List

main = withSocketsDo $ do
	putStrLn "Waiting..."
	let port = PortNumber 9001
	sockMy <- listenOn port
	listHandlersS <- newTVarIO []
	sockHandler sockMy listHandlersS listofHandlers

listofHandlers :: [Handle]
listofHandlers = []

sockHandler sock listStm list = do
	(handIO, _, _) <- accept sock
	putStrLn "Connected!"
	fromHandle <- openFile "handlers" AppendMode
	hPutStrLn fromHandle (show handIO)
	hClose fromHandle
	list <- atomically(readTVar listStm)
	atomically $ addClient handIO (addHandle handIO list) listStm
	forkIO $ command listStm handIO
	sockHandler sock listStm (addHandle handIO list)


addHandle :: Handle -> [Handle] -> [Handle]
addHandle x xs = xs ++ [x]

addClient hand handlers listHandlersStm = do
	writeTVar listHandlersStm handlers

command :: TVar [Handle] -> Handle -> IO ()
command handlersStm handler = do
	doMany handlersStm handler
	hClose handler
	return ()
	
doMany handlersStm handler = do
		line <- hGetLine handler
		handlers <- atomically (readTVar handlersStm)
		if(line=="/quit") then do
			name <- hGetLine handler
			broadcast handlers (name ++ " leaving") handler
			atomically(writeTVar handlersStm (delete handler handlers))
			return ()
		else do	
			atomically(writeTVar handlersStm handlers)
			if handlers == [] then return ()
			else do
				broadcast handlers line handler
				doMany handlersStm handler
		
broadcast handlers line handler= do
	if(handler/=(head handlers)) then do 
		doInOut (head handlers) line
		if(tail handlers /= []) then broadcast (tail handlers) line handler
		else return ()
	else do
		doInOut (head handlers) ""
		if(tail handlers /= []) then broadcast (tail handlers) line handler
		else return ()
	

doInOut :: Handle -> String -> IO()
doInOut handle line = hPutStrLn handle (line)
	

	
	
import Network
import System.IO
import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM

main = withSocketsDo $ do
	putStrLn "Hi 1"
	let port = PortNumber 9001
	sockMy <- listenOn port
	putStrLn "Hi 2"
	listHandlersS <- newTVarIO []
	sockHandler sockMy listHandlersS listofHandlers


listofHandlers :: [Handle]
listofHandlers = []

--listHandlers :: STM(TVar[Handle])


sockHandler sock listStm list = do
	putStrLn "Hi 3"
	(handIO, _, _) <- accept sock
	putStrLn "Connected!"
	fromHandle <- openFile "handlers" AppendMode
	hPutStrLn fromHandle (show handIO)
	hClose fromHandle 
	--let listHandlers :: STM(TVar[Handle])
	putStrLn "listStm before"
	--print(length(atomically(readTVar listStm)))
	atomically $ addClient handIO (addHandle handIO list) listStm
	putStrLn "listStm after"
	--print(length(atomically(readTVar listStm)))
	forkIO $ command listStm handIO
	sockHandler sock listStm (addHandle handIO list)


addHandle :: Handle -> [Handle] -> [Handle]
addHandle x xs = xs ++ [x]

addClient hand handlers listHandlersStm = do
	writeTVar listHandlersStm handlers
	--return listHandlersStm

command :: TVar [Handle] -> Handle -> IO ()
command handlersStm handler = do
	putStrLn "Hi 5"
	putStrLn "Hi 4"
	doMany handlersStm handler
	putStrLn "Hi 9"
	--hPutStr handle contents
	putStrLn "Hi 10"
	command handlersStm handler
	
doMany handlersStm handler = do
	handlers <- atomically (readTVar handlersStm)
	print(length(handlers))
	atomically(writeTVar handlersStm handlers)
	if handlers == [] then return ()
	else do
		--putStrLn unlines head handlers
		print (length(tail handlers))
		print(handlers)
		putStrLn "Hi in doMany"
		line <- hGetLine handler
		print(head handlers)
		broadcast handlers line
		doMany handlersStm handler
		
broadcast handlers line = do
	doInOut (head handlers) line
	putStrLn "Hi after doInOut head"
	print (tail handlers)
	if(tail handlers /= []) then broadcast (tail handlers) line
	else return ()

doInOut :: Handle -> String -> IO()
doInOut handle line = do
	putStrLn "Hi 6"
	--putStrLn line
	putStrLn "Hi after hGetContents before stdout hPutStr 8"
	--fromHandle2 <- openFile "chat.txt" ReadMode
	--contents <- hGetContents fromHandle2
	--hPutStr stdout contents
	--putStrLn line
	hPutStrLn handle (line ++ " 2")
	
--toAllClients message handle = do
	--hPutStr handle (atomically (readTVar message))
	
	
	
--Нужно: получение строки от клиента и отправка ее. + 
--Отправка всем подключенным пользователям
	
	
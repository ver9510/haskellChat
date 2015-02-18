import Network
import System.IO
import Control.Concurrent

main = withSocketsDo $ do
	putStrLn "Hi 1"
	let port = PortNumber 9001
	sockMy <- listenOn port
	putStrLn "Hi 2"
	sockHandler sockMy
	
	
sockHandler sock = do
	let listofHandlers=[]
	putStrLn "Hi 3"
	(handIO, _, _) <- accept sock
	putStrLn "Connected!"
	forkIO $ command handIO
	sockHandler sock

command handle= do
	putStrLn "Hi 5"
	putStrLn "Hi 4"
	line <- hGetLine handle
	putStrLn "Hi 6"
	--putStrLn line
	putStrLn "Hi after hGetContents before stdout hPutStr 8"
	--fromHandle2 <- openFile "chat.txt" ReadMode
	--contents <- hGetContents fromHandle2
	--hPutStr stdout contents
	--putStrLn line
	hPutStrLn handle (line ++ " 2")
	
	putStrLn "Hi 9"
	--hPutStr handle contents
	putStrLn "Hi 10"
	command handle
	
	
	
--Нужно: получение строки от клиента и отправка ее. Отправка всем подключенным пользователям
	
	

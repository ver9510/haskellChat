import Network
import System.IO
import Control.Concurrent

main = withSocketsDo $ do
	
	let port = PortNumber 9001
	handleReceive <- connectTo "localhost" port
	putStrLn "Connected!"
	putStrLn "Hi 2"
	putStrLn "Введите логин: "
	login <- getLine
	--putStr "Вводите текст: " 
	text login handleReceive
	return ()

text name handle= do
	putStr "> "
	line <- getLine
	if (null line)
		then return ()
		else do	
			--fromHandle <- openFile "chat.txt" AppendMode
			hPutStrLn handle (name ++ ": " ++ line)
			--hPutStr fromHandle (name ++ ": " ++ line++"\n")
			printFile handle
			putStrLn "Hi 5"
			text name handle			
			

printFile handle = do
	contents <- hGetLine handle
	hPutStrLn stdout contents
	putStrLn "Hi 8"

input handle = do
	line <-getLine
	putStrLn "Hi 6"
	hPutStrLn handle line
	putStrLn "Hi 5"
	content <- hGetLine handle
	putStrLn "Hi 3"
	putStrLn content
	putStrLn "Hi 4"
	input handle
--Отправка строки на сервер
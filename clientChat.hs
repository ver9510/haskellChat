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
	forkIO $ printFile handleReceive
	text login handleReceive
	--inOutputLoop login handleReceive
	return ()

text name handle= do
	--hFlush handle
	putStr "> "
	line <- getLine
	if (null line)
		then return ()
		else do	
			--fromHandle <- openFile "chat.txt" AppendMode
			hPutStrLn handle (name ++ ": " ++ line)
			--hPutStr fromHandle (name ++ ": " ++ line++"\n")
			--printFile handle
			putStrLn "Hi 5"
			text name handle			
			

printFile handle = do
	--hSetBuffering handle NoBuffering
	contents <- hGetLine handle
	hPutStrLn stdout contents
	putStrLn "Hi 8"
	printFile handle

	
	
inOutputLoop name handle = do
	putStrLn "before isEOF "
	isEOF <- hIsEOF handle 
	print isEOF
	if(isEOF /= True) then do 
		putStrLn "in inOutputLoop isEOF = false"
		printFile handle
	else do
		putStrLn "in inOutputLoop isEOF = true"
		putStr "> "
		line <- getLine
		if (null line)
			then return ()
			else do	
				--fromHandle <- openFile "chat.txt" AppendMode
				hPutStrLn handle (name ++ ": " ++ line)
	inOutputLoop name handle	
	
text2 name handle= do
	--hFlush handle
	putStr "> "
	line <- getLine
	if (null line)
		then return ()
		else do	
			--fromHandle <- openFile "chat.txt" AppendMode
			hPutStrLn handle (name ++ ": " ++ line)
			--hPutStr fromHandle (name ++ ": " ++ line++"\n")
			--printFile handle
			loopOut handle
			putStrLn "Hi 5"
			text2 name handle		

loopOut handle = do
	isReadable <- hIsReadable handle
	putStr "isRead = "
	print isReadable
	isWriteable <- hIsWritable handle
	putStr "isWrite = "
	print isWriteable
	isReady <- hReady handle
	putStr "isReady = "
	print isReady
	putStrLn "before isEOF "
	isEOF <- hIsEOF handle 
	print isEOF
	
	if(isEOF /= True) then do 
		putStrLn "in inOutputLoop isEOF = false"
		printFile handle
		loopOut handle
	else return ()
	
input name handle = do
	line <-getLine
	putStrLn "Hi 6"
	--hPutStrLn handle line
	hPutStrLn handle (name ++ ": " ++ line)
	putStrLn "Hi 5"
	content <- hGetLine handle
	putStrLn "Hi 3"
	putStrLn content
	putStrLn "Hi 4"
	input name handle
--Отправка строки на сервер



--попробовать проверять не на конец файла- хэндла. а на его доступность, или как он открыт..
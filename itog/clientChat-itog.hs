import Network
import System.IO
import Control.Concurrent

main = withSocketsDo $ do
	
	let port = PortNumber 9001
	handleReceive <- connectTo "localhost" port
	putStrLn "Connected!"
	putStrLn "Введите логин: "
	login <- getLine
	forkIO $ printFile handleReceive
	text login handleReceive
	hClose handleReceive
	return ()

text name handle= do
	--putStr "> "
	line <- getLine
	if (null line)
		then do 
			hPutStrLn handle ("/quit")
			hPutStrLn handle (name)
			putStrLn "Exiting.."
			return ()
		else do
			hPutStrLn handle (name ++ ": " ++ line)
			text name handle	
			

printFile handle = do
	x <-hIsOpen handle
	if(x==True) then do
		contents <- hGetLine handle
		hPutStrLn stdout contents
		printFile handle
	else return ()
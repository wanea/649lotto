module Main where
import ActionFunction (chooseMode, randTicket, wantTicket, welcome, welcomeChooseMode,
                       welcomeCustomMode, welcomeRandomMode, winTicket)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Function (checkIfWin)

main :: IO ()
main = do
    welcome
    >> main'

main' :: IO ()
main' = do
    way <- welcomeChooseMode >> chooseMode
    if way then welcomeCustomMode >> customMode
    else welcomeRandomMode >> randomMode

customMode :: IO ()
customMode = do
                myTicket <- runMaybeT wantTicket
                putStrLn $ "Your custom ticket is : " ++ show myTicket
                winnerTicket <- runMaybeT randTicket
                putStrLn $ "The Winning ticket is : " ++ show winnerTicket
                let lst = checkIfWin myTicket winnerTicket
                putStr $ "You found " ++ show (length lst) ++ " Number(s) : "
                print lst >> main'

randomMode :: IO ()
randomMode = do
                myTicket <- runMaybeT randTicket
                putStrLn $ "The Random ticket is  : " ++ show myTicket
                winnerTicket <- runMaybeT winTicket
                putStrLn $ "The Winning ticket is : " ++ show winnerTicket
                let lst = checkIfWin myTicket winnerTicket
                putStr $ "You found " ++ show (length lst) ++ " Number(s) : "
                print lst >> main'



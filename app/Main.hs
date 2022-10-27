module Main where
import ActionFunction
import Control.Monad.Trans.State
import DataTest
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

                myTicket <-  wantTicket
                putStrLn $ "Your custom ticket is : " ++ show myTicket
                winnerTicket <-  randTicket
                putStrLn $ "The Winning ticket is : " ++ show winnerTicket
                let lst = checkIfWin myTicket winnerTicket
                putStr $ "You found " ++ show (length lst) ++ " Number(s) : "
                print lst >> main'

randomMode :: IO ()
randomMode = do
                myTicket <- randTicket
                putStrLn $ "The Random ticket is  : " ++ show myTicket
                winnerTicket <- winTicket
                putStrLn $ "The Winning ticket is : " ++ show winnerTicket
                let lst = checkIfWin myTicket winnerTicket
                putStr $ "You found " ++ show (length lst) ++ " Number(s) : "
                print lst >> main'



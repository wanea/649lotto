module Main where
import ActionFunction (addBallT, addBallTRand, chooseMode, emptyTicketT, randTicket, wantTicket,
                       welcome, winTicket)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Function (checkIfWin)
import Type (Ball (Ball), Ticket (Ticket))




main :: IO ()
main = do
    welcome
    way <- chooseMode
    if way  then
                do
                myTicket <- runMaybeT wantTicket
                putStrLn $ "Your custom ticket is : " ++ show myTicket
                winnerTicket <- runMaybeT randTicket
                putStrLn $ "The Winning ticket is : " ++ show winnerTicket
                let lst = checkIfWin myTicket winnerTicket
                putStr $ "You found " ++ show (length lst) ++ " Number(s) : "
                print lst

            else
                do
                myTicket <- runMaybeT randTicket
                putStrLn $ "The Random ticket is  : " ++ show myTicket
                winnerTicket <- runMaybeT winTicket
                putStrLn $ "The Winning ticket is : " ++ show winnerTicket
                let lst = checkIfWin myTicket winnerTicket
                putStr $ "You found " ++ show (length lst) ++ " Number(s) : "
                print lst >> main



----------------  Variable de test ------------

aBall :: Ball Int
aBall = Ball 5
aBall' :: Ball Int
aBall' = Ball 60
mBall ::Maybe (Ball Int)
mBall = Just $ Ball 5
mBall' ::Maybe (Ball Int)
mBall' = Just $ Ball 1
aTicket :: Ticket (Ball Int)
aTicket = Ticket [Ball 1,Ball 2, Ball 3, Ball 4,Ball 5]
aTicket' :: Ticket (Ball Int)
aTicket' = Ticket [Ball 1,Ball 2, Ball 3, Ball 4,Ball 5,Ball 6]
saTicket :: Ticket (Ball Int)
saTicket = Ticket [Ball 5]
eTicket :: Maybe (Ticket a)
eTicket = Just $ Ticket []
mTicket :: Maybe (Ticket (Ball Int))
mTicket = Just $ Ticket [Ball 1,Ball 2,Ball 3, Ball 4, Ball 6, Ball 8]
mTicket1 :: Maybe (Ticket (Ball Int))
mTicket1 = Just $ Ticket [Ball 23,Ball 3, Ball 4, Ball 6, Ball 8]
eTicketT :: MaybeT IO (Ticket (Ball Int))
eTicketT = pure aTicket
fullTicketT :: MaybeT IO (Ticket (Ball Int))
fullTicketT = pure aTicket'
aTicketT :: MaybeT IO (Ticket (Ball Int))
aTicketT = pure aTicket

module Main where
import ActionFunction
import Function
import Type


-- scenario control

aBall :: Ball Int
aBall = Ball 5

aTicket :: Ticket (Ball Int)
aTicket = Ticket [Ball 6,Ball 8,Ball 49, Ball 27, Ball 8, Ball 34]
eTicket :: Maybe (Ticket a)
eTicket = Just $ Ticket []

main :: IO ()
main = do
    welcome
    way <- chooseMode
    if way  then
                do
                x <- wantTicket eTicket
                putStrLn "This is your Ticket : NEED TO CHECK REDONDUNCY"
                print x
            else
                do
                putStrLn "The crash is comming"
                x <- randTicket
                putStrLn "This is your random ticket : TO DO "
                print x
    putStrLn "The Winning ticket is : "
    winnerTicket <- randTicket
    print winnerTicket


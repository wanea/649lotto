module ActionFunction where
import Function (choiceToBall, convBallToTicket, validateBall, validateYorN)

import Type
--All IO Functions

welcome :: IO()
welcome = do
          putStrLn "-------------------------------------------------------"
          putStrLn "|     You are Welcome in this small Lotto project     |"
          putStrLn "|                                                     |"
          putStrLn "|     Two way :                                       |"
          putStrLn "|                                                     |"
          putStrLn "| 1 - Choose Balls one by one to make your Ticket     |"
          putStrLn "| 2 - Choose a random Ticket                          |"
          putStrLn "-------------------------------------------------------"

chooseMode :: IO Bool
chooseMode = do
  validateYorN <$> getLine

chooseTxtBall :: Int -> IO ()
chooseTxtBall num = putStr $ "Choice ball Number " ++ show num ++ " : "

--readChoice :: IO (Maybe Int)
readChoice :: IO (Maybe (Ticket (Ball Int)))
readChoice = do
  convBallToTicket . choiceToBall . validateBall <$> getLine

wantTicket :: Maybe (Ticket (Ball Int)) -> IO (Maybe (Ticket (Ball Int)))
wantTicket mTicket = do
      if enought <= 6
        then
          do
          chooseTxtBall enought
          addBallToTicket mTicket
        else pure mTicket
      where enought = 1 + (length.getTicket) ticket
            (Just ticket) = mTicket

addBallToTicket ::Maybe (Ticket (Ball Int)) -> IO (Maybe (Ticket (Ball Int)))
addBallToTicket mTicket =  do
          add <- readChoice
          case mTicket of
            (Just x) -> wantTicket $ mTicket <> add
            Nothing  -> wantTicket add

randTicket :: IO (Maybe (Ticket (Ball Int)))
randTicket = (pure.pure) (Ticket [Ball 6,Ball 8,Ball 49, Ball 27, Ball 8, Ball 34])

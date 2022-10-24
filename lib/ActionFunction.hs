module ActionFunction where
import Control.Monad.Trans.Maybe (MaybeT (..))
import Function (addBallToTicket, choiceToBall, isFullTicket, validateBall, validateYorN)
import System.Random ()
import Type (Ball (Ball), Ticket (Ticket))
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

--- selecting mode Custom Ticket or flash Ticket
chooseMode :: IO Bool
chooseMode = do
  validateYorN <$> getLine

readChoiceT ::MaybeT IO (Ball Int)
readChoiceT = MaybeT $ do
    print "Choose a ball [1-49]: "
    mInt <- validateBall <$> getLine
    case mInt of
        Nothing    -> print "Not valid entry or already choosed " >>  runMaybeT  readChoiceT
        (Just int) -> pure $ choiceToBall mInt

addBallT :: MaybeT IO (Ticket (Ball Int)) -> MaybeT IO (Ticket (Ball Int))
addBallT mTicketT =  do
  bool <- isFullTicketT mTicketT
  if bool
    then mTicketT
  else do
    ticket <- mTicketT
    aBall <-  readChoiceT
    addBallT (addBallToTicket aBall <$> mTicketT)

isFullTicketT ::MaybeT IO (Ticket (Ball Int)) ->MaybeT IO Bool
isFullTicketT = fmap isFullTicket


randTicket :: MaybeT IO (Ticket (Ball Int))
randTicket = pure (Ticket [Ball 6,Ball 42,Ball 49, Ball 27, Ball 8, Ball 34])

randTicket' ::MaybeT IO  (Ticket (Ball Int))
randTicket' = pure (Ticket [Ball 45,Ball 33,Ball 49, Ball 18, Ball 8, Ball 34])

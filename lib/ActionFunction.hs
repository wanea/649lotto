module ActionFunction where
import Control.Monad.Trans.Maybe (MaybeT (..))
import Function (addBallToTicket, choiceToBall, isFullTicket, lengthOfTicket, validateBall,
                 validateYorN)

import System.Random (randomRIO)
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
          putStrLn "| 2 - Choose a random Ticket (Default)                |"
          putStrLn "-------------------------------------------------------"

--- selecting mode Custom Ticket or flash Ticket
chooseMode :: IO Bool
chooseMode = do
  validateYorN <$> getLine

emptyTicketT :: MaybeT IO (Ticket (Ball Int))
emptyTicketT = pure $ Ticket []

readChoiceT ::MaybeT IO (Ball Int)
readChoiceT = MaybeT $ do
    print "Choose a ball [1-49]: "
    mInt <- validateBall <$> getLine
    case mInt of
        Nothing    -> print "Not valid entry :"
                      >>  runMaybeT  readChoiceT
        (Just int) -> pure $ choiceToBall mInt

randChoiceT :: MaybeT IO (Ball Int)
randChoiceT = MaybeT $ do
        num <- randomRIO (1,49)
        pure $ Just (Ball num)


addBallT :: MaybeT IO (Ticket (Ball Int)) -> MaybeT IO (Ticket (Ball Int))
addBallT mTicketT =  do
  bool <- isFullTicketT mTicketT
  if bool
    then  mTicketT
  else do
    ticket <- mTicketT
    aBall <-  readChoiceT
    addBallT (addBallToTicket aBall <$> mTicketT)

addBallTRand :: MaybeT IO (Ticket (Ball Int)) -> MaybeT IO (Ticket (Ball Int))
addBallTRand mTicketT =  do
  bool <- isFullTicketT mTicketT
  if bool
    then  mTicketT
  else do
    ticket <- mTicketT
    aBall <-  randChoiceT
    addBallTRand (addBallToTicket aBall <$> mTicketT)

isFullTicketT ::MaybeT IO (Ticket (Ball Int)) ->MaybeT IO Bool
isFullTicketT = fmap isFullTicket

wantTicket :: MaybeT IO (Ticket (Ball Int))
wantTicket = addBallT emptyTicketT

randTicket :: MaybeT IO (Ticket (Ball Int))
randTicket = addBallTRand emptyTicketT

winTicket ::MaybeT IO  (Ticket (Ball Int))
winTicket = randTicket

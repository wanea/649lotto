{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
module ActionFunction where
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Function (addBallToTicket, checkIfWin, choiceToBall, isFullTicket, validateBall,
                 validateYorN)
import System.Random
import Type (Ball (Ball), Credit, Ticket (Ticket))
--All IO Functions

welcome :: IO()
welcome = do
  putStrLn "-------------------------------------------------------"
  putStrLn "     You are Welcome in this small Lotto project      "
  putStrLn "-------------------------------------------------------"

welcomeCustomMode :: IO ()
welcomeCustomMode = do
  putStrLn "====================CUSTOM MODE========================"

welcomeRandomMode :: IO ()
welcomeRandomMode = do
  putStrLn "====================RANDOM MODE========================"

welcomeChooseMode :: IO ()
welcomeChooseMode = do
  putStrLn " "
  putStr   "Custom [1] or Random [2] (default) : "



--- selecting mode Custom Ticket or flash Ticket
chooseMode :: IO Bool
chooseMode = do
  validateYorN <$> getLine

emptyTicket :: Ticket (Ball Int)
emptyTicket = Ticket []

readChoice :: IO (Ball Int)
readChoice = do
    print "Choose a ball [1-49]: "
    mInt <- validateBall <$> getLine
    case mInt of
        Nothing    -> print "Not valid entry :"
                      >> readChoice
        (Just int) -> pure $ Ball int

randChoice :: IO (Ball Int)
randChoice = do
  num <- randomRIO (1,49)
  pure $ Ball num

randChoice' :: IO (Ball Int)
randChoice' = do
  num <- randomRIO (1,49)
  pure $ Ball num

addBall :: Ticket (Ball Int) -> IO (Ticket (Ball Int))
addBall ticket =  do
  if isFullTicket ticket
    then pure ticket
  else do
    aBall <- readChoice
    addBall (addBallToTicket aBall ticket)

addBallRand :: Ticket (Ball Int) -> IO (Ticket (Ball Int))
addBallRand ticket =  do
  if isFullTicket ticket
    then pure ticket
  else do
    aBall <- randChoice
    addBallRand (addBallToTicket aBall ticket)

wantTicket :: IO (Ticket (Ball Int))
wantTicket = addBall emptyTicket

randTicket :: IO (Ticket (Ball Int))
randTicket = addBallRand emptyTicket

winTicket :: IO (Ticket (Ball Int))
winTicket = randTicket

--withCredit :: StateT Credit IO (Ticket (Ball Int))
--withCredit  = StateT $ do (\c -> (a , c))
minus1 :: Int -> Int
minus1 x = x-1

makeState :: StateT Credit IO (Ticket (Ball Int))
makeState = StateT  (\input -> do
    pure (Ticket [], input )
    )

--takeState :: StateT Credit IO (Ticket(Ball Int)) ->IO Credit
--takeState (StateT ps) =(\input -> do
--                                  (_,s0) <- (ps input)
--                                  s0
--  )

playState :: StateT Credit IO (Ticket (Ball Int)) -> StateT Credit IO (Ticket (Ball Int))
playState ps = StateT (\input ->  do
  ticket <- randTicket
  ( _ , s1) <- runStateT ps input
  putStrLn $ "The Random ticket is  : " ++ show ticket
  winnerTicket <- winTicket
  putStrLn $ "The Winning ticket is : " ++ show winnerTicket
  let lst = checkIfWin ticket winnerTicket
  putStr $ "You found " ++ show (length lst) ++ " Number(s) : "
  putStrLn $ "remaining credit : " ++ (show (minus1 s1))
  pure (ticket , minus1 s1)
  )


enougthCredit ::StateT Credit IO (Ticket (Ball Int)) -> IO Bool
enougthCredit ps =  undefined





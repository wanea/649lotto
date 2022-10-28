module ActionFunction where

import Control.Monad.Trans.State (StateT (..))
import Function (addBallToTicket, checkIfWin, choiceToBall, isFullTicket, validateBall,
                 validateBall', validateYorN)
import System.Random (randomRIO)
import Type (Ball (Ball), Credit, Ticket (Ticket))
{-=============================================================================
=                          Printable Function
==============================================================================-}
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

printWinTicket :: Ticket (Ball Int) -> IO ()
printWinTicket ticket = do
  putStrLn $ "The Winning ticket is : " ++ show ticket

printRandomTicket :: Ticket (Ball Int) -> IO ()
printRandomTicket ticket = do
  putStrLn "------------------------------------------------"
  putStrLn $ "The Random ticket is  : " ++ show ticket

printCustomTicket :: Ticket (Ball Int) -> IO ()
printCustomTicket ticket = do
  putStrLn $ "Your custom ticket is : " ++ show ticket
  putStrLn "------------------------------------------------"

printBallFound :: [Ball Int] -> IO ()
printBallFound xs = do
  putStrLn ""
  putStrLn $ "You find " ++ show (length xs) ++ " Ball(s) : "
  putStrLn ""

printCredit :: Credit -> IO()
printCredit c = do
  putStrLn $ show c ++ " Credit remaining !"
{-=============================================================================
=                           Game Control Function
==============================================================================-}
chooseMode :: IO Bool
chooseMode = do
  validateYorN <$> getLine
{-=============================================================================
=                               Game Function
==============================================================================-}
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
{-=============================================================================
=                      All StateT  function
==============================================================================-}
startCredit :: StateT Credit IO (Ticket a)
startCredit = StateT  (\_ -> do
    pure ( Ticket [] , 3 )
    )

addCredit ::Int -> StateT Credit IO (Ticket a) -> StateT Credit IO (Ticket a)
addCredit c ps = StateT  (\input -> do
    (ps', input ) <- runStateT ps input
    pure (ps', input + c )
    )

getCredit :: StateT Credit IO (Ticket (Ball Int)) -> StateT Credit IO (Ticket (Ball Int))
getCredit rt = StateT (\input ->
   do
    (rt' ,cred) <- runStateT rt input
    pure (rt' , cred )
  )

playLottoR :: StateT Credit IO (Ticket (Ball Int)) -> StateT Credit IO (Ticket (Ball Int))
playLottoR ps = StateT (\input ->  do
  rTicket <- randTicket
  wTicket <- winTicket
  let xs = checkIfWin rTicket wTicket
  printRandomTicket rTicket
  printWinTicket wTicket
  printBallFound xs
  ( _ , s1) <- runStateT ps input
  pure (rTicket ,s1 - 1)
  )

playLottoC :: StateT Credit IO (Ticket (Ball Int)) -> StateT Credit IO (Ticket (Ball Int))
playLottoC ps = StateT (\input ->  do
  rTicket <- wantTicket
  wTicket <- winTicket
  let xs = checkIfWin rTicket wTicket
  printRandomTicket rTicket
  printWinTicket wTicket
  printBallFound xs
  ( _ , s1) <- runStateT ps input
  pure (rTicket ,s1 - 1)
  )

gameLottoPlayR ::StateT Credit IO (Ticket (Ball Int)) -> IO ()
gameLottoPlayR ps = do
  (x, y) <- runStateT ps 0
  printCredit y
  if y > 0
    then gameLottoPlayR $ playLottoR ps
    else print "GAME OVER"
  pure() -- or return, which makes more sense ?

gameLottoPlayC ::StateT Credit IO (Ticket (Ball Int)) -> IO ()
gameLottoPlayC ps = do
  (x, y) <- runStateT ps 0
  printCredit y
  print $ "Etat :::::::::::: " ++ show y
  if y > 0
    then gameLottoPlayC  ps
    else print "GAME OVER"
  pure() -- or return, which makes more sense ?



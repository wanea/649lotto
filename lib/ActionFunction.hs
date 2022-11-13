module ActionFunction where

import Control.Monad.Trans.State (StateT (..))
import Function (addBallToTicket, checkIfWin, isFullTicket, sortedTicket, validateBall,
                 validateYorN)
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
  putStrLn $ "You find " ++ show (length xs) ++ " Ball(s) : " ++ show xs
  putStrLn ""

printCredit :: Credit -> IO()
printCredit c = do
  putStrLn $ "[ " ++ show c ++ " ] Credit remaining !"

gameOver  :: IO()
gameOver = putStrLn "---------/!\\ GAME OVER /!\\---------"
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
    print "Choose a ball between 1 and 49 : "
    mInt <- validateBall <$> getLine
    case mInt of
        Nothing    -> print "Invalid entry : "
                      >> readChoice
        (Just int) -> pure $ Ball int

randChoice :: IO (Ball Int)
randChoice = do
  num <- randomRIO (1,49)
  pure $ Ball num


addBall :: Ticket (Ball Int) -> IO (Ticket (Ball Int))
addBall ticket =  do
  if isFullTicket ticket
    then pure (sortedTicket ticket)
  else do
    aBall <- readChoice
    addBall (addBallToTicket aBall ticket)

addBallRand :: Ticket (Ball Int) -> IO (Ticket (Ball Int))
addBallRand ticket =  do
  if isFullTicket ticket
    then pure (sortedTicket ticket)
  else do
    aBall <- randChoice
    addBallRand $ addBallToTicket aBall ticket

wantTicket  :: IO (Ticket (Ball Int))
wantTicket  = addBall emptyTicket

randTicket  :: IO (Ticket (Ball Int))
randTicket  = addBallRand emptyTicket

winTicket   :: IO (Ticket (Ball Int))
winTicket   = randTicket
{-=============================================================================
=                      All StateT  function
==============================================================================-}
startCredit  :: StateT Credit IO (Ticket a)
startCredit   = StateT  (\_ -> do pure ( Ticket [] , 3 ))

addCredit :: StateT Credit IO a -> StateT Credit IO  a
addCredit  ps = StateT  (\input -> do
    (ps', input' ) <- runStateT ps input
    print $ show input ++ " CREDIT ADDED "
    pure (ps', input+input' )
    )

sub1Credit :: StateT Credit IO a -> StateT Credit IO a
sub1Credit ps = StateT (\cred -> do
  (ps', cred') <- runStateT ps cred
  pure (ps', cred' - 1)
  )

manageCredit :: StateT Credit IO a -> StateT Credit IO a
manageCredit  =  addCredit . sub1Credit

discover :: Ticket (Ball Int) -> IO Int
discover ticket = do
  wTicket <- winTicket
  printWinTicket wTicket
  let xs = checkIfWin ticket wTicket
  printBallFound xs
  pure $ length xs

playLottoR :: IO Int
playLottoR  =  do
  rTicket <- randTicket
  printRandomTicket rTicket
  discover rTicket

gameLottoPlayR ::StateT Credit IO (Ticket (Ball Int)) -> IO ()
gameLottoPlayR ps = do
  (x, y) <- runStateT ps 0
  printCredit y
  if y > 0
    then do
      wCred <- playLottoR
      (_, aCred) <- runStateT (manageCredit ps) wCred
      let nState = StateT  (\_ -> do pure ( x , aCred))
      gameLottoPlayR nState
    else gameOver
  return()

playLottoC :: IO Int
playLottoC  = do
  cTicket <- wantTicket
  printCustomTicket cTicket
  discover cTicket

gameLottoPlayC ::StateT Credit IO (Ticket (Ball Int)) -> IO ()
gameLottoPlayC ps = do
  (x, y) <- runStateT ps 0
  printCredit y
  if y > 0
    then  do
      wCred <- playLottoC
      (_, aCred) <- runStateT (manageCredit ps) wCred
      let nState = StateT  (\_ -> do pure ( x , aCred))
      gameLottoPlayC nState
    else gameOver
  return()

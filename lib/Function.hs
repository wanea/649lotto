module Function where

import Data.Char (isNumber)
import Data.List (sort)
import Type (Ball (..), Ticket (..))

validateYorN:: String -> Bool
validateYorN "" = False
validateYorN str
          | isNumber x && x == '1' = True
          | otherwise   = False
          where (x:xs) = str

-- check if input have all requirement
validateBall :: String -> Maybe Int
validateBall "" = Nothing
validateBall str
          | isNum str && validRange = pure convInt
          | otherwise = Nothing
        where convInt = read str
              validRange = convInt > 0 && convInt < 50

validateBall':: String ->IO (Maybe Int)
validateBall' str
    | num && range = pure mInt
    | num && not range = print "Not in Range" >> pure  Nothing
    | not num && range = print "Not a Number" >> pure Nothing
    |otherwise = pure Nothing
      where (Just num, Just range) = (validateNum' str,validateRange' str)
            mInt = read str

validateRange' :: String -> Maybe Bool
validateRange' ""  = Nothing
validateRange' str = pure $ convInt > 0 && convInt < 50
    where convInt = read str

validateNum' :: String -> Maybe Bool
validateNum' ""                  = Nothing
validateNum' str
  | convInt > 0 && convInt < 50 = Just True
  | otherwise                   = Just False
     where convInt = read str

isNum :: [Char] -> Bool
isNum ""     = True
isNum (x:xs) = isNumber x && isNum xs

addBallToTicket :: Ball Int -> Ticket (Ball Int) -> Ticket (Ball Int)
addBallToTicket ball ticket
          | ballIsPresent ball ticket = ticket
          | otherwise = pure ball <> ticket

ballIsPresent :: Ball Int -> Ticket (Ball Int) -> Bool
ballIsPresent b t = b `elem` getTicket t

choiceToBall ::Maybe Int -> Maybe (Ball Int)
choiceToBall = maybe Nothing (pure . pure)

isFullTicket :: Ticket a -> Bool
isFullTicket ticket
          | len < 6 = False
          |otherwise = True
          where (Just len) = lengthOfTicket (Just ticket)

lengthOfTicket :: Maybe (Ticket a) ->Maybe Int
lengthOfTicket mTicket = length <$> (getTicket <$> mTicket)

checkIfWin :: Ticket (Ball Int) -> Ticket (Ball Int) -> [Ball Int]
checkIfWin (Ticket mylist) (Ticket winlist) =
  sortListBall [ x | x <- mylist , x `elem` winlist ]

sortedTicket ::  Ticket (Ball Int) -> Ticket (Ball Int)
sortedTicket ticket = Ticket $ Ball <$> sort (getBall <$> getTicket ticket)

sortListBall :: [Ball Int] -> [Ball Int]
sortListBall xs = Ball <$> sorted
      where sorted = sort  $ fmap getBall xs

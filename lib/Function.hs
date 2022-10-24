module Function where
import Data.Char (isNumber, toLower)
import Data.List (sort)
import Type (Ball (..), Ticket (..))

validateYorN:: String -> Bool
validateYorN str
          | isNumber x && x == '1' = True
          | otherwise   = False
          where (x:xs) = str

-- check if input have all requirement
validateBall :: String -> Maybe Int
validateBall str
          | notEmpty str && isNum str && validRange = pure convInt
          | otherwise = Nothing
        where convInt = read str
              validRange = convInt > 0 && convInt < 50

notEmpty ::String -> Bool
notEmpty str
          | str == "" = False
          | otherwise = True

isNum :: [Char] -> Bool
isNum ""     = True
isNum (x:xs) = isNumber x && isNum xs

addBallToTicket :: Ball Int -> Ticket (Ball Int) -> Ticket (Ball Int)
addBallToTicket ball ticket
          | ballIsPresent ball ticket = ticket
          | otherwise = ballToTicket ball <> ticket

ballIsPresent :: Ball Int -> Ticket (Ball Int) -> Bool
ballIsPresent b t = b `elem` getTicket t

ballToTicket :: Ball Int -> Ticket (Ball Int)
ballToTicket ball = Ticket [ball]

choiceToBall ::Maybe Int -> Maybe (Ball Int)
choiceToBall = maybe Nothing (pure . pure)

isFullTicket :: Ticket a -> Bool
isFullTicket mTicket
          | len < 6 = False
          |otherwise = True
          where len = length $ getTicket  mTicket

lengthOfTicket :: Maybe (Ticket a) ->Maybe Int
lengthOfTicket mTicket = length <$> (getTicket <$> mTicket)

checkIfWin :: Maybe (Ticket (Ball Int)) -> Maybe (Ticket (Ball Int)) -> [Ball Int]
checkIfWin myTicket winTicket = do
  let (Just mylist) = fmap  getTicket myTicket
  let (Just winlist) = fmap  getTicket winTicket
  sortListBall [ x | x<- mylist , x `elem` winlist ]

sortListBall :: [Ball Int] -> [Ball Int]
sortListBall xs = Ball <$> sorted
      where sorted = sort  $ fmap getBall xs

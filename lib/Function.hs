module Function where
import Data.Char (isNumber, toLower)
import Data.List (sort)
import Type (Ball, Ticket (Ticket, getTicket))


-- all pure function
-- P

validateYorN:: String -> Bool
validateYorN str
  | isNumber x && x == '1' = True
  | otherwise   = False
  where (x:xs) = str


validateBall :: String -> Maybe Int
validateBall str = if notEmpty && isNum str && validRange  then pure convInt else Nothing
        where convInt = read str
              validRange = convInt > 0 && convInt < 50
              isNum ""     = True
              isNum (x:xs) = isNumber x && isNum xs
              notEmpty     = str /= ""

choiceToBall ::Maybe Int -> Maybe (Ball Int)
choiceToBall = maybe Nothing (pure . pure)

convBallToTicket :: Maybe (Ball Int)  -> Maybe (Ticket (Ball Int))
convBallToTicket   = maybe Nothing (pure . pure)






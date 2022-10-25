module DataTest where
import Control.Monad.Trans.Maybe
import Type

    ----------------  Variable de test ------------

aBall :: Ball Int
aBall = Ball 5
aBall' :: Ball Int
aBall' = Ball 60
mBall ::Maybe (Ball Int)
mBall = Just $ Ball 5
mBall' ::Maybe (Ball Int)
mBall' = Just $ Ball 1
aTicket :: Ticket (Ball Int)
aTicket = Ticket [Ball 19,Ball 36, Ball 13, Ball 41,Ball 55]
aTicket' :: Ticket (Ball Int)
aTicket' = Ticket [Ball 1,Ball 2, Ball 3, Ball 4,Ball 5,Ball 6]
saTicket :: Ticket (Ball Int)
saTicket = Ticket [Ball 5]
eTicket :: Maybe (Ticket a)
eTicket = Just $ Ticket []
mTicket :: Maybe (Ticket (Ball Int))
mTicket = Just $ Ticket [Ball 1,Ball 2,Ball 3, Ball 4, Ball 6, Ball 8]
mTicket1 :: Maybe (Ticket (Ball Int))
mTicket1 = Just $ Ticket [Ball 23,Ball 3, Ball 4, Ball 6, Ball 8]
eTicketT :: MaybeT IO (Ticket (Ball Int))
eTicketT = pure aTicket
fullTicketT :: MaybeT IO (Ticket (Ball Int))
fullTicketT = pure aTicket'
aTicketT :: MaybeT IO (Ticket (Ball Int))
aTicketT = pure aTicket

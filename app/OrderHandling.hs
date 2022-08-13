{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module OrderHandling where

{-
OrderBook contains the unmatched Bids and Offers
The OrderBook will always be sorted in price-time priority
 -}


import Control.Monad
import Control.Monad.State
import Data.List

type Price = Float
type Size = Float
data Side = Bid | Ofr deriving (Show, Eq)
type TraderID = String
type Transaction = Order

data Order = Order {
                     traderID    :: TraderID,
                     orderTime   :: Int,
                     orderSide  :: Side,
                     orderPrice :: Price,
                     orderSize  :: Size
                   }

data Trade = Trade {
                     buyerID    :: TraderID,
                     sellerID   :: TraderID,
                     tradePrice :: Price,
                     tradeSize  :: Size
                   }

instance Show Order where
  show (Order tID oTime Bid oPrice oSi) = concat [show tID ,  " Buy " , show oSi, "@", show oPrice]
  show (Order tID oTime Ofr oPrice oSi) = concat [show tID ,  " Sell " , show oSi, "@", show oPrice]

instance Show Trade where
  show t = concat [show (buyerID t), " bought ", show (tradeSize t), " from ", show (sellerID t), " at price ", show (tradePrice t), " per unit"]

instance Eq Order where
  oa == ob = (orderSide oa == orderSide ob) && (orderPrice oa == orderPrice ob) && (orderTime oa == orderTime ob)

instance Ord Order where
  compare oa ob
          | oa == ob = EQ
          | orderSide oa == orderSide ob = case compare (orderPrice oa) (orderPrice ob) of
                                            EQ -> compare (orderTime oa) (orderTime ob)
                                            GT -> case orderSide oa of
                                                       Bid -> GT
                                                       Ofr -> LT
                                            LT -> case orderSide oa of
                                                       Bid -> LT
                                                       Ofr -> GT
          | orderSide oa /= orderSide ob = error "Cannot compare orders from the contra side"

data OrderBook = OrderBook {
                      buySide     :: [Order],
                      sellSide    :: [Order],
                      nextOrderTime :: Int
                    }

instance Show OrderBook where
  show ob  =  concat ["\n====================\nBuy side\n", showSide (buySide ob), "\nSell Side\n", showSide (sellSide ob)]
               where
                --showSide :: [a] -> String
                showSide [] = []
                showSide (x:xs) = concat [show x, "\n", showSide xs]



createEmptyOrderBook :: OrderBook
createEmptyOrderBook = OrderBook [] [] 0

--temporary function to load an orderbook. This function will eventually load a file into an OrderBook type
loadOrderBook :: OrderBook
loadOrderBook = OrderBook [or1, or3] [or2, or4] 5
                 where
                   or1 = Order {traderID = "OJ", orderTime = 1, orderSide = Bid, orderPrice = 10, orderSize = 1}
                   or2 = Order {traderID = "BK", orderTime = 2, orderSide = Ofr, orderPrice = 11, orderSize = 1}
                   or3 = Order {traderID = "SD", orderTime = 3, orderSide = Bid, orderPrice = 10.5, orderSize = 1}
                   or4 = Order {traderID = "IA", orderTime = 4, orderSide = Ofr, orderPrice = 10.75 , orderSize = 1}



topOfBookMatch :: OrderBook -> Bool
topOfBookMatch ob = bestBuyPrice >= bestSellPrice
 where
  bestBuyPrice = orderPrice $ bestBuyOrder ob
  bestSellPrice = orderPrice $ bestSellOrder ob

bestBuyOrder :: OrderBook -> Order
bestBuyOrder ob = maximum (buySide ob)

bestSellOrder :: OrderBook -> Order
bestSellOrder ob = maximum (sellSide ob)

mkTradesOrderbook :: State OrderBook [Trade]
mkTradesOrderbook = do
        oBook0  <- get
        if topOfBookMatch oBook0 then
          do
           put $ OrderBook (init (buySide oBook0)) (init (sellSide oBook0)) (nextOrderTime oBook0)
           return  [createTrade oBook0]
        else
          return []
    where
      createTrade ob = Trade { buyerID    = traderID $ bestBuyOrder ob
                             , sellerID   = traderID $ bestSellOrder ob
                             , tradePrice = orderPrice $ bestSellOrder ob
                             , tradeSize  = minimum [orderSize (bestBuyOrder ob) , orderSize (bestSellOrder ob)]
                             }


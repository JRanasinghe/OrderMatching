module Main where
import OrderHandling

--import Tests
import System.Console.ANSI
import System.IO
import Control.Monad
import Control.Monad.State
    ( MonadState(get, put), MonadTrans(lift), StateT(runStateT), State, runState )
import Data.List
import OrderHandling (createEmptyOrderBook)


main :: IO ()
main = do
    clearScreen
    putStrLn "Welcome the Batch-64 Trading System by Janaka!\n"
    putStrLn "==============================================\n"
    putStrLn "\nType \"l\" to load the test orderbook or any key to create new orderbook"
    commandM <- getLine
    case commandM of
        "l" -> runStateT orderMenu loadOrderBook
        otherwise -> runStateT orderMenu createEmptyOrderBook
    return ()

orderMenu :: StateT OrderBook IO ()
orderMenu =
    do
     lift $ putStrLn "\nChoose from the Options below"
     lift $ putStrLn "Type \"o\" to Enter a new order"
     lift $ putStrLn "Type \"v\" to view the top of the orderbook"
     --putStrLn "Type \"t\" to view all trades from this session"
     lift $ putStrLn "Type \"q\" to quit"
     commandO <- lift getLine
     oBook0 <- get
     case commandO of
        "o" -> do
            lift $ putStrLn "Enter order"
            newOrder <- lift mkOrder
            lift $ print newOrder
            let (t, newOrderBook) = runState (insertAndMatch newOrder) oBook0 in
             do
              put newOrderBook
              lift $ print t
              orderMenu
        "v" -> do
            lift $ print oBook0
            orderMenu
        "q" -> do
            lift $ putStrLn "Good Bye!"
        otherwise -> do
            lift $ putStrLn "\nPlease enter a valid command"
            orderMenu

mkOrder :: IO Order
mkOrder = do
    putStrLn "Enter the unique ID of the Trader:"
    tID  <- getLine
    putStrLn "Type \"b\" to Buy or any key to Sell (s)? :"
    oSideString <- getLine
--    putStrLn "Enter Quantity to trade:"
--    oSizeString <-  getLine
    putStrLn "Enter Price per unit: "
    oPriceString <- getLine
    return $ Order { traderID = tID
                   , orderTime = 0 -- at this point the priority is 0, it will be added when inserting into the Orderbook
                   , orderSide = case oSideString of
                                    "b" -> Bid
                                    otherwise -> Ofr
                   , orderSize = 1 -- in this version all orders will be of size 1 
                   , orderPrice = read oPriceString :: Price
                   }

insertAndMatch :: Order -> State OrderBook [Trade]
insertAndMatch nO = do
    OrderBook bSide sSide nextT  <- get
    let newOrder = Order { traderID  = traderID nO
                         , orderTime = nextT
                         , orderSide = orderSide nO
                         , orderSize = orderSize nO
                         , orderPrice = orderPrice nO
                         } in
      put $ OrderBook { buySide = if orderSide newOrder == Bid then insert newOrder bSide else bSide
                      , sellSide = if orderSide newOrder == Ofr then insert newOrder sSide else sSide
                      , nextOrderTime = nextT + 1
                      }
    oBook1 <- get
    if [] `elem` [buySide oBook1,sellSide oBook1] then
        return []
    else
         mkTradesOrderbook




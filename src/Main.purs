module Main where

import Prelude
import Global

import Control.Apply ((*>))
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Data.Functor (($>))

import Halogen
import Halogen.Query.StateF (modify, gets)
import Halogen.Util (appendToBody)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E

type State = { monthlyRent :: Number, propertyPrice :: Number, grossYield :: Number, deposit :: Number }

initialState :: State
initialState = { monthlyRent: 0.0, propertyPrice: 0.0, grossYield: 0.0, deposit: 0.0 }

calculateYield :: Number -> Number -> Number
calculateYield monthlyRent propertyPrice = monthlyRent * 12.0 / propertyPrice * 100.0

calculateLTV :: State -> Number
calculateLTV st = (st.propertyPrice - st.deposit) / st.propertyPrice * 100.0

data Input a
  = UpdatePropertyValue String a
  | UpdateMonthlyRent String a
  | UpdateDeposit String a

ui :: forall g p. (Functor g) => Component State Input g p
ui = component render eval
    where

    render :: Render State Input p
    render st =
      H.div_  [ H.h1_ [ H.text "Yield Calculator" ]
              , H.p_  [ H.text "Property Value"
                      , H.input [ P.type_ "text"
                                , P.placeholder "Property Price"
                                , P.value $ show st.propertyPrice
                                , E.onValueChange (E.input UpdatePropertyValue)
                                ]
                      ]
              , H.p_  [ H.text "Rent (monthly)"
                      , H.input [ P.type_ "text"
                                , P.placeholder "Monthly Rent"
                                , P.value $ show st.monthlyRent
                                , E.onValueChange (E.input UpdateMonthlyRent)
                                ]
                      ]
              , H.p_  [ H.text "Deposit"
                      , H.input [ P.type_ "text"
                                , P.placeholder "Deposit"
                                , P.value $ show st.deposit
                                , E.onValueChange (E.input UpdateDeposit)
                                ]
                      ]
              , H.table_  [ H.tr_ [ H.td_ [H.text "Gross Yield"], H.td_ [ H.text (show st.grossYield) ] ]
                          , H.tr_ [ H.td_ [H.text "Loan to Value"], H.td_ [ H.text (show $ calculateLTV st) ] ]
                          ]
              ]

    eval :: Eval Input State Input g
    eval (UpdateMonthlyRent rent next) = do
      let rent' = readFloat rent
      propertyPrice <- gets (\st -> st.propertyPrice)
      modify (_ { monthlyRent = rent', grossYield = calculateYield rent' propertyPrice })
      return next
    eval (UpdatePropertyValue price next) = do
      let price' = readFloat price -- modify (\st -> recalcWithPrice st $ readFloat price) $> next
      rent <- gets \st -> st.monthlyRent
      modify (_ { propertyPrice = price', grossYield = calculateYield rent price' })
      return next
    eval (UpdateDeposit deposit next) = modify (_ { deposit = readFloat deposit }) $> next

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
    app <- runUI ui initialState
    appendToBody app.node

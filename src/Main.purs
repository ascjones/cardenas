module Main where

import Prelude
import Global

import Control.Apply ((*>))
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Data.Functor (($>))

import Halogen
import Halogen.Query.StateF (modify)
import Halogen.Util (appendToBody)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E

type State = { propertyValue :: Number, deposit :: Number, monthlyRent :: Number, grossYield :: Number }

initialState :: State
initialState = { monthlyRent: 0.0, propertyValue: 0.0, deposit: 0.0, grossYield: 0.0 }

calculateYield :: Number -> Number -> Number
calculateYield monthlyRent propertyValue = monthlyRent * 12.0 / propertyValue * 100.0

calculateLTV :: State -> Number
calculateLTV st = (st.propertyValue - st.deposit) / st.propertyValue * 100.0

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
                                , P.placeholder "Property Value"
                                , P.value $ show st.propertyValue
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
              , H.p_  [ H.text "Deposit Amount"
                      , H.input [ P.type_ "text"
                                , P.placeholder "Deposit Amount"
                                , P.value $ show st.deposit
                                , E.onValueChange (E.input UpdateDeposit)
                                ]
                      ]
              , H.ul_ [ H.li_ [ H.p_ [ H.text ("Gross Yield" ++ show st.grossYield) ] ]
                      , H.li_ [ H.p_ [ H.text ("Loan To Value") ] ]
                      ]
              ]

    eval :: Eval Input State Input g
    eval (UpdateMonthlyRent rent next) = 
      modify (_ { monthlyRent: readFloat rent, grossYield: 0.0 })-- modify (\st -> { grossYield: calculateYield (readFloat rent) st.propertyValue }) $> next --(\st -> recalcWithRent st $ readFloat rent) $> next
    eval (UpdatePropertyValue price next) = modify (\st -> recalcWithPrice st $ readFloat price) $> next

    recalcWithRent :: State -> Number -> State
    recalcWithRent st rent = { monthlyRent: rent, propertyValue: st.propertyValue,  grossYield: calculateYield rent st.propertyValue, deposit: 0.0 }

    recalcWithPrice :: State -> Number -> State
    recalcWithPrice st price = { monthlyRent: st.monthlyRent, propertyValue: price, grossYield: calculateYield st.monthlyRent price, deposit: 0.0 }

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
    app <- runUI ui initialState
    appendToBody app.node

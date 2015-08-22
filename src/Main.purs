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

type State = { monthlyRent :: Number, propertyPrice :: Number, grossYield :: Number }

initialState :: State
initialState = { monthlyRent: 0.0, propertyPrice: 0.0, grossYield: 0.0 }

calculateYield :: Number -> Number -> Number
calculateYield monthlyRent propertyPrice = monthlyRent * 12.0 / propertyPrice * 100.0

data Input a
    = UpdateMonthlyRent String a
    | UpdatePropertyPrice String a

ui :: forall g p. (Functor g) => Component State Input g p
ui = component render eval
    where

    render :: Render State Input p
    render st =
      H.div_  [ H.h1_ [ H.text "Yield Calculator" ]
              , H.p_  [ H.text "Rent (monthly)"
                      , H.input [ P.type_ "text"
                                , P.placeholder "Monthly Rent"
                                , P.value $ show st.monthlyRent
                                , E.onValueChange (E.input UpdateMonthlyRent)
                                ]
                      ]
              , H.p_  [ H.text "Property Value"
                      , H.input [ P.type_ "text"
                                , P.placeholder "Property Price"
                                , P.value $ show st.propertyPrice
                                , E.onValueChange (E.input UpdatePropertyPrice)
                                ]
                      ]
              , H.p_ [ H.text (show st.grossYield) ]
              ]

    eval :: Eval Input State Input g
    eval (UpdateMonthlyRent rent next) = modify (\st -> recalcWithRent st $ readFloat rent) $> next
    eval (UpdatePropertyPrice price next) = modify (\st -> recalcWithPrice st $ readFloat price) $> next

    recalcWithRent :: State -> Number -> State
    recalcWithRent st rent = { monthlyRent: rent, propertyPrice: st.propertyPrice,  grossYield: calculateYield rent st.propertyPrice }

    recalcWithPrice :: State -> Number -> State
    recalcWithPrice st price = { monthlyRent: st.monthlyRent, propertyPrice: price, grossYield: calculateYield st.monthlyRent price }

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
    app <- runUI ui initialState
    appendToBody app.node

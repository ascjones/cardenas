module Main where

import Prelude
import Global

import Control.Apply ((*>))
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Data.Functor (($>))
import Data.Maybe (Maybe(..))

import Halogen
import Halogen.Query.StateF (modify, gets)
import Halogen.Util (appendToBody)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E

type State = { monthlyRent :: Maybe Number, propertyValue :: Maybe Number, deposit :: Maybe Number }

initialState :: State
initialState = { monthlyRent: Nothing, propertyValue: Nothing, deposit: Nothing }

calculateYield :: State -> Maybe Number
calculateYield st = do
  rent <- st.monthlyRent
  propertyValue <- st.propertyValue
  return $ rent * 12.0 / propertyValue * 100.0

calculateLTV :: State -> Maybe Number
calculateLTV st = do
  propertyValue <- st.propertyValue
  deposit <- st.deposit
  return $ (propertyValue - deposit) / propertyValue * 100.0

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
              , H.p_  [ H.text "Deposit"
                      , H.input [ P.type_ "text"
                                , P.placeholder "Deposit"
                                , P.value $ show st.deposit
                                , E.onValueChange (E.input UpdateDeposit)
                                ]
                      ]
              , H.table_  [ H.tr_ [ H.td_ [H.text "Gross Yield"], H.td_ [ H.text (show $ calculateYield st) ] ]
                          , H.tr_ [ H.td_ [H.text "Loan to Value"], H.td_ [ H.text (show $ calculateLTV st) ] ]
                          ]
              ]

    eval :: Eval Input State Input g
    eval (UpdateMonthlyRent rent next)    = modify (_ { monthlyRent = Just $ readFloat rent }) $> next
    eval (UpdatePropertyValue price next) = modify (_ { propertyValue = Just $ readFloat price }) $> next
    eval (UpdateDeposit deposit next)     = modify (_ { deposit = Just $ readFloat deposit }) $> next

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
    app <- runUI ui initialState
    appendToBody app.node

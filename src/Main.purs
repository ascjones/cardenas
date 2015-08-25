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

import Model

data Input a
  = UpdatePropertyValue String a
  | UpdateMonthlyRent String a
  | UpdateDeposit String a
  | UpdateInitialCosts String a
  | UpdateMortgageInterestRate String a
  | ToggleUseAgency Boolean a

ui :: forall g p. (Functor g) => Component State Input g p
ui = component render eval
    where

    render :: Render State Input p
    render st =
      H.div_  [ H.h1_ [ H.text "Mortgage Calculator" ]
              , H.ol_ [ H.li_ [ H.label [ P.for "propertyValue" ] [ H.text  "Property Value" ]
                              , H.input [ P.id_ "propertyValue"
                                        , P.type_ "text"
                                        , P.placeholder "Property Price"
                                        , P.value $ showNumber st.propertyValue
                                        , E.onValueChange (E.input UpdatePropertyValue)
                                        ]
                          ]
                      , H.li_ [ H.label [ P.for "monthlyRent" ][ H.text  "Rent (Monthly)" ]
                              , H.input [ P.id_ "monthlyRent"
                                        , P.type_ "text"
                                        , P.placeholder "Monthly Rent"
                                        , P.value $ showNumber st.monthlyRent
                                        , E.onValueChange (E.input UpdateMonthlyRent)
                                        ]
                              ]
                      , H.li_ [ H.label [ P.for "deposit" ] [ H.text  "Deposit" ]
                              , H.input [ P.id_ "deposit"
                                        , P.type_ "text"
                                        , P.placeholder "Deposit"
                                        , P.value $ showNumber st.deposit
                                        , E.onValueChange (E.input UpdateDeposit)
                                        ]
                              ]
                      , H.li_ [ H.label [ P.for "initialCosts" ] [ H.text  "Initial Costs" ]
                              , H.input [ P.id_ "initialCosts"
                                        , P.type_ "text"
                                        , P.placeholder "Initial Costs"
                                        , P.value $ showNumber st.initialCosts
                                        , E.onValueChange (E.input UpdateInitialCosts)
                                        ]
                              ]
                      , H.li_ [ H.label [ P.for "useAgency" ] [ H.text  "Agency Managed (15%)" ]
                              , H.input [ P.id_ "useAgency"
                                        , P.type_ "checkbox"
                                        , P.title "Use Agency"
                                        , P.checked st.useAgency
                                        , E.onChecked (E.input ToggleUseAgency)
                                        ]
                              ]
                      , H.li_ [ H.label [ P.for "interestRate" ] [ H.text  "Mortgage Interest Rate" ]
                              , H.input [ P.id_ "interestRate"
                                        , P.type_ "text"
                                        , P.placeholder "Mortgage Interest Rate"
                                        , P.value $ showNumber st.interestRate
                                        , E.onValueChange (E.input UpdateMortgageInterestRate)
                                        ]
                              ]
                      ]
              , H.table_  [ H.tr_ [ H.td_ [H.text "Gross Yield"], H.td_ [ H.text (showNumber $ calculateGrossYield st) ] ]
                          , H.tr_ [ H.td_ [H.text "Loan to Value"], H.td_ [ H.text (showNumber $ calculateLTV st) ] ]
                          , H.tr_ [ H.td_ [H.text "Monthly Mortgage Payment"], H.td_ [ H.text (showNumber $ calculateMonthlyMortgagePayment st) ] ]
                          , H.tr_ [ H.td_ [H.text "Net Yield"], H.td_ [ H.text (showNumber $ calculateNetYield st) ] ]
                          ]
              ]

    showNumber :: Maybe Number -> String
    showNumber Nothing = ""
    showNumber (Just n) = show n

    eval :: Eval Input State Input g
    eval (UpdateMonthlyRent rent next) =
      modify (_ { monthlyRent = Just $ readFloat rent }) $> next
    eval (UpdatePropertyValue price next) =
      modify (_ { propertyValue = Just $ readFloat price }) $> next
    eval (UpdateDeposit deposit next) =
      modify (_ { deposit = Just $ readFloat deposit }) $> next
    eval (UpdateInitialCosts initialCosts next) =
      modify (_ { initialCosts = Just $ readFloat initialCosts }) $> next
    eval (UpdateMortgageInterestRate interestRate next) =
      modify (_ { interestRate = Just $ readFloat interestRate }) $> next
    eval (ToggleUseAgency b next) =
      modify (_ { useAgency = b }) $> next

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
    app <- runUI ui initialState
    appendToBody app.node

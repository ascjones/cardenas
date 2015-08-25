module Model where

import Prelude
import Data.Maybe (Maybe(..))

type State =
  { monthlyRent :: Maybe Number
  , propertyValue :: Maybe Number
  , deposit :: Maybe Number
  , interestRate :: Maybe Number
  , initialCosts :: Maybe Number
  , useAgency :: Boolean
  }

initialState :: State
initialState =
  { monthlyRent: Nothing
  , propertyValue: Nothing
  , deposit: Nothing
  , interestRate: Nothing
  , initialCosts: Nothing
  , useAgency: false
  }

calculateYield :: Number -> Number -> Number -> Number
calculateYield rent investment ongoingCosts =
  (rent - ongoingCosts) * 12.0  / investment * 100.0

calculateGrossYield :: State -> Maybe Number
calculateGrossYield st = do
  rent <- st.monthlyRent
  propertyValue <- st.propertyValue
  return $ calculateYield rent propertyValue 0.0

calculateNetYield :: State -> Maybe Number
calculateNetYield st = do
  rent <- st.monthlyRent
  propertyValue <- st.propertyValue
  initialCosts <- st.initialCosts
  let agencyCosts = if st.useAgency then rent * 0.15 else 0.0
  return $ calculateYield rent (propertyValue + initialCosts) agencyCosts

calculateInitialInvestment :: State -> Maybe Number
calculateInitialInvestment st = (+) <$> st.initialCosts <*> st.deposit

calculateLTV :: State -> Maybe Number
calculateLTV st = do
  propertyValue <- st.propertyValue
  deposit <- st.deposit
  return $ (propertyValue - deposit) / propertyValue * 100.0

calculateMonthlyMortgagePayment :: State -> Maybe Number
calculateMonthlyMortgagePayment st = do
  propertyValue <- st.propertyValue
  interestRate <- st.interestRate
  deposit <- st.deposit
  return $ (propertyValue - deposit) * (interestRate / 100.0) / 12.0

module Main where

import Prelude
import Control.Monad.Eff.Console

calculateYield :: Number -> Number -> Number
calculateYield monthlyRent propertyPrice = monthlyRent * 12.0 / propertyPrice * 100.0 

-- main = do
--  log "Hello sailor!"

module Main where

import Prelude (Unit, ($), (<>), show)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Euler (answer)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ "The answer is: " <> (show answer)

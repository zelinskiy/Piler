module HomePage.Effects where

import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff.Console (CONSOLE)

type Effects fx =
  ( ajax :: AJAX
  , console :: CONSOLE
  , dom :: DOM | fx)

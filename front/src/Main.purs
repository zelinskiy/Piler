module Main where

import Control.Bind ((=<<), bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Function ((<<<))
import Data.Unit (Unit)
import Pux (start)
import DOM.HTML (window)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM)
import Signal.Channel (CHANNEL)
import Signal ((~>))

import App (Event(Navigate), Effects, foldp, view, init)
import Routes(match)

main :: forall fx. Eff
        ( channel :: CHANNEL
        , exception :: EXCEPTION | Effects fx) Unit
main = do
  urlSignal <- sampleURL =<< window
  let routeSignal = urlSignal ~> (Navigate <<< match)
  
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: [routeSignal]
    }

  renderToDOM "#app" app.markup app.input


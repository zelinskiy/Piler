module Main where

import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Unit (Unit)
import DOM (DOM)
import Pux (start)
import Pux.Renderer.React (renderToDOM)
import Signal.Channel (CHANNEL)
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff.Console (CONSOLE)

import Login.App (foldp, view, init)

main :: forall fx. Eff
        ( console :: CONSOLE
        , ajax :: AJAX
        , channel :: CHANNEL
        , dom :: DOM
        , exception :: EXCEPTION | fx) Unit
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input


module Utils.Form ( DateTimeString(..)
                  , Dropdown(..)
                  , asDateTimeString
                  , asDropdown
                  ) where

import Prelude

import Pux.DOM.Events (DOMEvent, onChange, targetValue)
import Pux.Form (class Render, render)
import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes (type', value, selected)
import Text.Smolder.Markup ((!), (#!), text)

import Data.String(fromCharArray, toCharArray)
import Data.Array((!!), dropEnd)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Foldable(foldl)
import Data.Lens(Lens', lens)

foreign import targetSelectedIndex :: DOMEvent -> Int

newtype DateTimeString = DateTimeString String
derive instance newtypeDateTimeString :: Newtype DateTimeString _

-- We pass around DateTime's as something like
-- 2017-12-01T00:00:00Z
-- But html rejects it with the trailing Z,
-- so here is a workaround
instance renderDateTime :: Render DateTimeString where
  render a = 
    HTML.input
      ! type' "datetime-local"
      ! value (dropLast (unwrap a))
      #! onChange (wrap <<< flip (<>) "Z" <<< targetValue)
    where
      dropLast = fromCharArray <<< dropEnd 1 <<< toCharArray

asDateTimeString :: Lens' String DateTimeString
asDateTimeString = lens wrap $ const unwrap 

-- Dropdown with custom show function

data Dropdown a = Dropdown (a -> String) a (Array a)

instance renderDropdown :: (Eq a) => Render (Dropdown a) where
  render (Dropdown s val choices') =
    HTML.select
    #! (onChange \e->
         Dropdown s
         (fromMaybe val $ choices' !! targetSelectedIndex e)
         choices')
    $ foldl (*>) (text "") options
    where
      options =
        choices'
        <#> \c-> let elem = HTML.option $ text $ s c
                 in if c == val
                    then elem ! (selected "true")
                    else elem

asDropdown :: forall a. Eq a =>
              (a -> String) -> Array a -> Lens' a (Dropdown a)
asDropdown s vars =
  lens (\a-> Dropdown s a vars) (\_ (Dropdown _ a _)-> a)

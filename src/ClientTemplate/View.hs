{-# LANGUAGE QuasiQuotes #-}

module ClientTemplate.View where

import Text.RawString.QQ
import Data.Text as T

viewElm :: T.Text
viewElm = T.pack $ [r|module View exposing (..)

import Static.Types exposing (..)
import Html exposing(..)
import Browser as B

--change what's shown on the browser window / tab bar!
title : Model -> String
title model =
    "My New App!"

--change what's on the screen
view : Model -> Html ClientMessage
view model =
    text "Hello World!"|]
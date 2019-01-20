module NewspaperExample.View.MainStreet exposing(..)
import NewspaperExample.Static.Types.MainStreet exposing(Msg(..))
import NewspaperExample.Static.Types exposing(MainStreet(..))
import Html exposing(Html)
import Debug exposing(todo)

view : MainStreet -> Html Msg
view mainStreet =
  div []
  [ button [ onClick EnterEditingRoom ] [ text "Reading Room" ]
  , button [ onClick EnterReadingRoom ] [ text "Editing Room" ]
  ]

title : MainStreet -> String
title mainStreet =
    "Main Street"

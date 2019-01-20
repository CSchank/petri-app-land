module NewspaperExample.View.ReadingRoom exposing(..)
import NewspaperExample.Static.Types.ReadingRoom exposing(Msg(..))
import NewspaperExample.Static.Types exposing(ReadingRoom(..))
import Html exposing(Html)
import Debug exposing(todo)

view : ReadingRoom -> Html Msg
view readingRoom =
  div []
  [ button [ onClick LeaveReadingRoom ] [ text "Exit" ]
  -- EnterTitle
  -- EnterText
  ]

title : ReadingRoom -> String
title readingRoom =
    "Reading Room"

module NewspaperExample.View.EditingRoom exposing(..)
import NewspaperExample.Static.Types.EditingRoom exposing(Msg(..),getTitles)
import NewspaperExample.Static.Types exposing(EditingRoom(..))
import Html exposing(Html)
import Debug exposing(todo)

view : EditingRoom -> Html Msg
view editingRoom =
  div []
  [ button [ onClick LeaveEditingRoom ] [ text "Exit" ]
  , div [] [ text (String.join ", " <| getTitles editingRoom) ]
  , button [ onClick PublishArticle ] [ text "Publish" ]
  ]

title : EditingRoom -> String
title editingRoom =
    "Editing Room"

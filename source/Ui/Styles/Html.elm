module Ui.Styles.Html exposing (styledNode)

import Html.Attributes exposing (attribute)
import Css exposing (Snippet, Mixin)
import Html exposing (text)
import String

styledNode : String
     -> String
     -> List (Html.Attribute msg)
     -> List (Html.Html msg)
     -> List Mixin
     -> Html.Html msg
styledNode tag uid attributes content mixins =
  let
    sel =
      "[css=\"" ++ uid ++ "\"]"

    attr =
      attribute "css" uid

    baseSelector =
      Css.selector sel mixins

    --otherSelectors =
    --  List.map (\(part, mixns) -> Css.selector (sel ++ part) mixns) subs

    css =
      Css.stylesheet [baseSelector]
      |> Css.compile

    style =
      Html.node "style" [] [text css.css]
  in
    Html.node tag
      (attr :: attributes)
      (style :: content)

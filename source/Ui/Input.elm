module Ui.Input exposing
  (Model, Msg, init, subscribe, update, view, render, setValue)

{-| Component for single line text based input (wrapper for the input HTML tag).

# Model
@docs Model, Msg, init, subscribe, update

# View
@docs view, render

# Functions
@docs setValue
-}

import Html.Events exposing (onInput)
import Html exposing (node)
import Html.Lazy
import Html.Attributes
  exposing
    ( value
    , spellcheck
    , placeholder
    , type'
    , readonly
    , disabled
    , classList
    , attribute
    )

import Ui.Helpers.Emitter as Emitter
import Ui.Native.Uid as Uid

import String
import Task

import Ui.Styles.Theme as Theme exposing (default)
import Ui.Styles.Html exposing (styledNode)
import Css

{-| Representation of an input:
  - **placeholder** - The text to display when there is no value
  - **disabled** - Whether or not the input is disabled
  - **readonly** - Whether or not the input is readonly
  - **value** - The value
  - **kind** - The type of the input
  - **uid** - The unique identifier of the input
-}
type alias Model =
  { placeholder : String
  , disabled : Bool
  , readonly : Bool
  , value : String
  , kind : String
  , uid : String
  }


{-| Messages that an input can receive.
-}
type Msg
  = Input String


{-| Initializes an input with a default value and a placeholder.

    input = Ui.Input.init "value" "Placeholder..."
-}
init : String -> String -> Model
init value placeholder =
  { placeholder = placeholder
  , uid = Uid.uid ()
  , disabled = False
  , readonly = False
  , value = value
  , kind = "text"
  }


{-| Subscribe to the changes of an input.

    ...
    subscriptions =
      \model -> Ui.Input.subscribe InputChanged model.input
    ...
-}
subscribe : (String -> msg) -> Model -> Sub msg
subscribe msg model =
  Emitter.listenString model.uid msg


{-| Updates an input.

    Ui.Input.update msg input
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Input value ->
      ( setValue value model, Emitter.sendString model.uid value )


{-| Lazily renders an input.

    Ui.Input.view input
-}
view : Model -> Html.Html Msg
view model =
  Html.Lazy.lazy render model

styles =
  [ Css.display Css.inlineBlock
  , Css.children
    [ Css.selector "input"
      [ Css.backgroundColor default.colors.input
      , Css.fontFamily Css.inherit
      , Css.fontSize Css.inherit
      , Css.lineHeight (Css.px 16)
      , Css.padding2 (Css.px 6) (Css.px 9)
      , Css.height default.inputs.height
      , Css.width (Css.pct 100)
      , Theme.base default
      , Theme.border default
      , Theme.focused default
      ]
    , Css.selector "input[readonly]"
      [ Css.property "cursor" default.readonlyCursor ]
    , Css.selector "input[disabled]"
      [ Theme.disabled default ]
    ]
  ]

{-| Renders an input.

    Ui.Input.render input
-}
render : Model -> Html.Html Msg
render model =
  styledNode
    "ui-input"
    model.uid
    [ readonly model.readonly
    , disabled model.disabled
    ]
    [ node
        "input"
        [ placeholder model.placeholder
        , attribute "id" model.uid
        , readonly model.readonly
        , disabled model.disabled
        , value model.value
        , spellcheck False
        , type' model.kind
        , onInput Input
        ]
        []
    ]
    styles


{-| Sets the value of an input.

    Ui.Input.setValue "new value" input
-}
setValue : String -> Model -> Model
setValue value model =
  { model | value = value }

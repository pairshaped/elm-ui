module Ui.Tagger exposing
  (Tag, Model, Msg, init, subscribe, update, view, render, setValue)

{-| Component for displaying tags and handling it's events (adding / removing).

# Model
@docs Model, Msg, Tag, init, subscribe, update

# View
@docs view, render

# Functions
@docs setValue
-}

import String

import Html.Attributes exposing (classList)
import Html.Events.Extra exposing (onKeys)
import Html.Events exposing (onClick)
import Html exposing (node, text)
import Html.Keyed
import Html.Lazy
import Html.App

import Json.Decode as JD
import Json.Encode as JE

import Ui.Helpers.Emitter as Emitter
import Ui.Native.Uid as Uid
import Ui.IconButton
import Ui.Container
import Ui.Input
import Ui

import Ui.Styles.Theme as Theme exposing (default)
import Ui.Styles.Html exposing (styledNode)
import Css

{-| Represents a tag:
  - **label** - The label to display
  - **id** - The identifier of the tag
-}
type alias Tag =
  { label : String
  , id : String
  }


{-| Representation of a tagger component:
  - **input** - The model of the input field
  - **removeable** - Whether or not tags can be removed
  - **disabled** - Whether or not the tagger is disabled
  - **readonly** - Whether or not the tagger is readonly
  - **uid** - The unique identifier of the chooser
-}
type alias Model =
  { input : Ui.Input.Model
  , removeable : Bool
  , disabled : Bool
  , readonly : Bool
  , uid : String
  }


{-| Actions that a tagger can make.
-}
type Msg
  = Input Ui.Input.Msg
  | Remove String
  | Create


{-| Initializes a tagger.

    tagger = Ui.Tagger.init "Add a tag..."
-}
init : String -> Model
init placeholder =
  { input = Ui.Input.init "" placeholder
  , uid = Uid.uid ()
  , removeable = True
  , disabled = False
  , readonly = False
  }


{-| Subscribe to the changes of a tagger.

    Ui.Tagger.subscribe AddTag RemoveTag model.tagger
-}
subscribe : (String -> msg) -> (String -> msg) -> Model -> Sub msg
subscribe create remove model =
  Sub.batch
    [ Emitter.listenString (model.uid ++ "-create") create
    , Emitter.listenString (model.uid ++ "-remove") remove
    ]


{-| Updates a tagger.

    Ui.Tagger.update msg tagger
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    Input act ->
      let
        ( input, effect ) =
          Ui.Input.update act model.input
      in
        ( { model | input = input }, Cmd.map Input effect )

    Create ->
      let
        isEmpty =
          String.trim model.input.value
            |> String.isEmpty
      in
        if isEmpty then
          ( model, Cmd.none )
        else
          ( model, Emitter.sendString (model.uid ++ "-create") model.input.value )

    Remove id ->
      ( model, Emitter.sendString (model.uid ++ "-remove") id )


{-| Lazily renders a tagger.

    Ui.Tagger.view [tag1, tag2] tagger
-}
view : List Tag -> Model -> Html.Html Msg
view tags model =
  Html.Lazy.lazy2 render tags model


styles =
  [ Css.display Css.inlineBlock
  , Css.children
    [ Css.selector "ui-tagger-tags"
      [ Css.display Css.block
      , Css.margin2 Css.zero (Css.px -5)
      , Css.children
        [ Css.selector "ui-tagger-tag"
          [ Css.borderRadius default.borderRadius
          , Css.property "display" "inline-flex"
          , Css.alignItems Css.center
          , Css.lineHeight default.inputs.height
          , Css.fontWeight (Css.int 600)
          , Css.padding2 Css.zero (Css.px 10)
          , Css.margin (Css.px 5)
          , Css.children
            [ Css.selector "ui-icon"
              [ Css.verticalAlign Css.middle
              , Css.marginLeft (Css.px 10)
              , Css.fontSize (Css.px 22)
              ]
            ]
          ]
        ]
      ]
    ]
  ]

{-| Renders a tagger.

    Ui.Tagger.render [tag1, tag2] tagger
-}
render : List Tag -> Model -> Html.Html Msg
render tags model =
  let
    input =
      model.input

    isEmpty =
      String.trim input.value
        |> String.isEmpty

    updatedInput =
      { input
        | disabled = model.disabled
        , readonly = model.readonly
      }

    button =
      { disabled = model.disabled || model.readonly || isEmpty
      , readonly = False
      , kind = "primary"
      , size = "medium"
      , glyph = "plus"
      , side = "left"
      , text = ""
      }

    classes =
      classList
        [ ( "disabled", model.disabled )
        , ( "readonly", model.readonly )
        ]

    actions =
      Ui.enabledActions
        model
        [ onKeys
            [ ( 13, Create )
            ]
        ]
  in
    styledNode
      "ui-tagger"
      model.uid
      (classes :: actions)
      [ Ui.Container.row
          []
          [ Html.App.map Input (Ui.Input.view updatedInput)
          , Ui.IconButton.view Create button
          ]
      , Html.Keyed.node
          "ui-tagger-tags"
          []
          (List.map (renderTag model) tags)
      ]
      styles


{-| Sets the value of a taggers input.

    Ui.Tagger.setValue "" tagger
-}
setValue : String -> Model -> Model
setValue value model =
  { model | input = Ui.Input.setValue value model.input }


{-| Renders a tag.
-}
renderTag : Model -> Tag -> (String, Html.Html Msg)
renderTag model tag =
  let
    icon =
      if model.disabled || model.readonly || not model.removeable then
        text ""
      else
        Ui.icon
          "android-close"
          True
          ([ onClick (Remove tag.id)
           , onKeys [ ( 13, Remove tag.id ) ]
           ]
            ++ (Ui.tabIndex model)
          )
  in
    (tag.id, node
      "ui-tagger-tag"
      []
      [ text tag.label
      , icon
      ])

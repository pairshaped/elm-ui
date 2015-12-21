module Ui.Form where

import Signal exposing (forwardTo)
import Array exposing (Array)
import Color exposing (Color)

import Html.Attributes exposing (class)
import Html exposing (node, text)

import Ui.Input
import Ui.Checkbox
import Ui.Container
import Ui.ColorPicker
import Ui

type Field
  = Checkbox Ui.Checkbox.Model
  | Input Ui.Input.Model
  | ColorPicker Ui.ColorPicker.Model

type Action
  = InputAction String Ui.Input.Action
  | CheckboxAction String Ui.Checkbox.Action
  | ColorPickerAction String Ui.ColorPicker.Action

type alias Model =
  { fields : Array (String, String, Field)
  }

init : Model
init =
  { fields = Array.empty
  }

input : String -> String -> String ->
        (Ui.Input.Model -> Ui.Input.Model) -> Model -> Model
input name label value update model =
  addField (name, label, Input (update (Ui.Input.init value))) model

color : String -> String -> Color ->
        (Ui.ColorPicker.Model -> Ui.ColorPicker.Model) -> Model -> Model
color name label value update model =
  addField (name, label, ColorPicker (update (Ui.ColorPicker.init value))) model

checkbox : String -> String -> Bool ->
           (Ui.Checkbox.Model -> Ui.Checkbox.Model) -> Model -> Model
checkbox name label value update model =
  addField (name, label, Checkbox (update (Ui.Checkbox.init value))) model

addField : (String, String, Field) -> Model -> Model
addField field model =
  { model | fields = Array.push field model.fields }

updateCheckbox action checkbox =
  case action of
    CheckboxAction name act -> Ui.Checkbox.update act checkbox
    _ -> checkbox

updateInput action input =
  case action of
    InputAction name act -> Ui.Input.update act input
    _ -> input

updateColorPicker action color =
  case action of
    ColorPickerAction name act -> Ui.ColorPicker.update act color
    _ -> color

updateField key action (name, label, field) =
  if name == key then
    case field of
      Checkbox checkbox ->
        (name, label, Checkbox (updateCheckbox action checkbox))
      Input input ->
        (name, label, Input (updateInput action input))
      ColorPicker color ->
        (name, label, ColorPicker (updateColorPicker action color))
  else
    (name, label, field)

update action model =
  let
    name =
      case action of
        CheckboxAction name _ -> name
        InputAction name _ -> name
        ColorPickerAction name _ -> name
  in
    { model | fields = Array.map (\item -> updateField name action item) model.fields }

view address model =
  node "ui-form" []
    (Array.map (\item -> renderField address item) model.fields
      |> Array.toList)

renderField address (name, label, field) =
  let
    (orientation, input) =
      case field of
        Checkbox checkbox ->
          ("horizontal", Ui.Checkbox.view (forwardTo address (CheckboxAction name)) checkbox)
        Input input ->
          ("vertical", Ui.Input.view (forwardTo address (InputAction name)) input)
        ColorPicker color ->
          ("vertical", Ui.ColorPicker.view (forwardTo address (ColorPickerAction name)) color)
  in
    node "ui-form-control" [class orientation]
      [ node "ui-form-label" [] [text label]
      , input
      ]

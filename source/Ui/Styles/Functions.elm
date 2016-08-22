module Ui.Styles.Functions exposing (..)

import Color exposing (Color)
import Color.Mixing as Mix
import Ext.Color
import Debug exposing (log)

luma : Color -> Float
luma color =
  let
    { red, green, blue } = Color.toRgb color
  in
    (0.2126 * (toFloat red)  +
     0.7152 * (toFloat green) +
     0.0722 * (toFloat blue)) / 255


readable : Color -> Color
readable color =
  let
    { red, green, blue } = Color.toRgb color

    lighten =
      setLightness 0.85 color

    darken =
      setLightness 0.4 color

    lumaBg = luma color
    lumaLighten = luma lighten
    lumaDarken = luma darken

    lightenDiff =
      abs (lumaBg - lumaLighten)

    darkenDiff =
      abs (lumaBg - lumaDarken)
  in
    if lightenDiff > darkenDiff then
      lighten
    else
      darken


setLightness : Float -> Color -> Color
setLightness updatedLightness color =
  let
    { red, green, blue, alpha } = Color.toRgb color

    (hue, saturation, lightness) = Ext.Color.rgbToHsl red green blue
  in
    Color.hsla hue saturation updatedLightness alpha

module Ui.Styles.Functions exposing (..)

import Color exposing (Color)
import Color.Mixing as Mix
import Color.Manipulate
import Ext.Color
import Ext.Number exposing (roundTo)
import Debug exposing (log)

{-| 0..1 -}
luma : Color -> Float
luma color =
  let
    { red, green, blue } = Color.toRgb color
  in
    (0.299 * (toFloat red)  +
     0.587 * (toFloat green) +
     0.114 * (toFloat blue)) / 244


contrast : Color -> Color -> Float
contrast a b =
  let
    lumaA = luma a
    lumaB = luma b
  in
    roundTo 2 (1 - ((min lumaA lumaB) / (max lumaA lumaB)))


contrastColorOffset : Float -> Int -> Color -> Color -> Color
contrastColorOffset desiredContrast offset base color =
  let
    (_, _, rawLightness, _) = toHsl color

    lightness =
      round (rawLightness * 100)

    cantIterate =
      (nextLightness + offset) <= 0 || (nextLightness + offset) >= 100

    nextLightness =
      clamp 0 100 (lightness + offset)

    nextColor =
      setLightness ((toFloat nextLightness) / 100) color
  in
    if (contrast nextColor base) >= desiredContrast || cantIterate then
      nextColor
    else
      contrastColorOffset desiredContrast offset base nextColor



contrastColor : Float -> Color -> Color
contrastColor desiredContrast color =
  if luma color > 0.5 then
    contrastColorOffset desiredContrast -1 color color
  else
    contrastColorOffset desiredContrast 1 color color


readable : Float -> Color -> Color
readable desiredContrast color =
  contrastColor desiredContrast color


setLightness : Float -> Color -> Color
setLightness updatedLightness color =
  let
    (hue, saturation, lightness, alpha) = toHsl color
  in
    Color.hsla hue saturation updatedLightness alpha


toHsl : Color -> (Float, Float, Float, Float)
toHsl color =
  let
    { red, green, blue, alpha } = Color.toRgb color
    (hue, saturation, lightness) = Ext.Color.rgbToHsl red green blue
  in
    (hue, saturation, lightness, alpha)

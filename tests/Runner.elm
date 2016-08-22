module Main exposing (..)

import String
import Task
import ElmTest exposing (..)

import Ext.DateTests
import Ext.Number exposing (roundTo)

import Color
import Ui.Styles.Functions exposing (contrastColor, contrast)
import Debug exposing (log)

tests : Test
tests =
  suite "A Test Suite"
    [ Ext.DateTests.tests
    , colorF
    ]

assertContrast value color =
  let
    color2 = contrastColor value color
  in
    assertEqual value (contrast color2 color)

colorF =
  suite "Contrast Color"
    [ test "full" (assertContrast 1 Color.white)
    , test "none" (assertContrast 0 Color.white)
    , test "half" (assertContrast 0.5 Color.white)
    , test "quarter" (assertContrast 0.25 Color.white)
    , test "black full" (assertContrast 1 Color.black)
    ]

main =
  runSuite tests

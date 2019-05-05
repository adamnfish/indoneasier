module Views.Welcome exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)

welcome : Html Msg
welcome =
    div []
        [ h1 []
            [ "Select company type" ]
        , ul []
            [ li
                [ onClick ]
                [ text "Rice" ]
            ]
        ]

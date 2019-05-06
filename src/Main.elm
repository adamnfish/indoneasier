module Main exposing (..)

import Browser
import Html exposing (Html, text, div, ul, li, table, thead, tbody, th, td, nav, h1, h2, p, img, button, b, a, span)
import Html.Attributes exposing (src, class, href)
import Html.Events exposing (onClick)


---- MODEL ----

type Merger
    = Rice
    | Spice
    | RiceAndSpice
    | SiapFaji
    | Rubber
    | Oil

type Count = Count Int
type Bid = Bid Int
type Split
    = SingleCompany
    | Split Int Int

type Model
    = Welcome
    | CompanySize Merger
    | CostTable Merger Count
    | CompanySplit Merger Count Bid
    | Payments Merger Count Bid Split

init : ( Model, Cmd Msg )
init =
    ( Welcome, Cmd.none )


minPrice : Merger -> Int
minPrice merger =
    case merger of
        Rice ->
            20
        Spice ->
            25
        RiceAndSpice ->
            25
        SiapFaji ->
            35
        Rubber ->
            30
        Oil ->
            40

---- UPDATE ----


type Msg
    = SelectMerger Merger
    | SelectCount Merger Count
    | SelectBid Merger Count Bid
    | SelectSplit Merger Count Bid Split
    | GoHome


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectMerger merger ->
            ( CompanySize merger, Cmd.none )
        SelectCount merger count ->
            ( CostTable merger count, Cmd.none )
        SelectBid merger count bid ->
            ( CompanySplit merger count bid, Cmd.none )
        SelectSplit merger count bid split ->
            ( Payments merger count bid split, Cmd.none )
        GoHome ->
            ( Welcome, Cmd.none )
    



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ nav
            [ class "brown darken-1" ]
            [ div
                [ class "container" ]
                [ div [ class "nav-wrapper" ]
                    [ div
                        [ class "brand-logo" ]
                        [ text "IMM" ]
                    , ul
                        [ class "right" ]
                        (
                            if model /= Welcome then
                                [ li []
                                    [ a
                                        [ onClick GoHome ]
                                        [ text "Start again" ]
                                    ]
                                ]
                            else
                                []
                        )
                    ]
                ]
            ]

        , div
            [ class "container" ]
            [ div [ class "row" ]
                [ div []
                    [
                        case model of
                            Welcome ->
                                welcome
                            CompanySize merger ->
                                companySize merger
                            CostTable merger count ->
                                costTable merger count
                            CompanySplit merger count bid ->
                                companySplit merger count bid
                            Payments merger count bid split ->
                                payments merger count bid split
                    ]
                ]
            ]
        ]

welcome : Html Msg
welcome =
    div []
        [ h2 []
            [ text "Select company type" ]
        , div
            [ class "collection collection-links" ]
            [ a
                [ onClick ( SelectMerger Rice )
                , class "collection-item"
                ]
                [ text "Rice" ]
            , a
                [ onClick ( SelectMerger Spice )
                , class "collection-item"
                ]
                [ text "Spice" ]
            , a
                [ onClick ( SelectMerger RiceAndSpice )
                , class "collection-item"
                ]
                [ text "Rice / Spice (Siap Faji)" ]
            , a
                [ onClick ( SelectMerger SiapFaji )
                , class "collection-item"
                ]
                [ text "Siap Faji" ]
            , a
                [ onClick ( SelectMerger Rubber )
                , class "collection-item"
                ]
                [ text "Rubber" ]
            , a
                [ onClick ( SelectMerger Oil )
                , class "collection-item"
                ]
                [ text "Oil" ]
            ]
        ]

companySizeButton : Merger -> Int -> Html Msg
companySizeButton merger size =
    a
        [ onClick ( SelectCount merger ( Count size ) )
        , class "collection-item"
        ]
        [ text ( String.fromInt size ) ]

companySize : Merger -> Html Msg
companySize merger =
    let
        sizeSelection = companySizeButton merger
    in
        div []
            [ h2 []
                [ text "Company size" ]
            , div [ class "collection collection-links" ]
                ( List.map sizeSelection ( List.range 1 25 ) )
            ]

costTable : Merger -> Count -> Html Msg
costTable merger ( Count count ) =
    let
        pricePerItem = minPrice merger
        initial = count * pricePerItem
    in
        div []
            [ h2 []
                [ text "Price table" ]
            , div
                [ class "collection collection-links" ]
                ( List.map
                    (\i ->
                        let
                            bid = initial + (i * count)
                        in
                            a
                                [ onClick ( SelectBid merger ( Count count ) ( Bid bid ) )
                                , class "collection-item"
                                ]
                                [ b []
                                    [ text ( String.fromInt bid ) ]
                                , span
                                    [ class "grey-text" ]
                                    [ text "  : "
                                    , text ( String.fromInt ( bid // count ) )
                                    , text " × "
                                    , text ( String.fromInt count )
                                    , text ""
                                    ]
                                ]
                    )
                    ( List.range 0 150 )
                )
            ]

companySplit : Merger -> Count -> Bid -> Html Msg
companySplit merger ( Count count ) bid =
    div []
        [ h2 []
            [ text "Calculate payment" ]
        , div [ class "collection collection-links" ]
            (
                [ a
                    [ onClick ( SelectSplit merger ( Count count ) bid SingleCompany )
                    , class "collection-item"
                    ]
                    [ text "Single owner" ]
                ] ++ (
                    List.map
                        (\i ->
                            a
                                [ onClick ( SelectSplit merger ( Count count ) bid ( Split i ( count - i ) ) )
                                , class "collection-item"
                                ]
                                [ text ( String.fromInt i )
                                , text " / "
                                , text ( String.fromInt ( count - i ) )
                                ]
                        )
                        ( List.range 1 ( count // 2 ) )
                )
            )
        ]

payments : Merger -> Count -> Bid -> Split -> Html Msg
payments merger ( Count count ) ( Bid bid ) split =
    let
        message =
            case merger of
                Rice ->
                    "Rice"
                Spice ->
                    "Spice"
                RiceAndSpice ->
                    "Rice / spice"
                SiapFaji ->
                    "Siap faji"
                Rubber ->
                    "Rubber"
                Oil ->
                    "Oil"
    in
        div []
            [ h2 []
                [ text "Payment" ]
            ,
                case split of
                    SingleCompany ->
                        p []
                            [ text "Single player gets the full bid "
                            , b []
                                [ text "("
                                , text ( String.fromInt bid )
                                , text ")"
                                ]
                            ]
                    Split player1Count player2Count ->
                        let
                            player1 = (bid // count) * player1Count
                            player2 = (bid // count) * player2Count
                        in
                            div []
                                [ p []
                                    [ text "Total cost "
                                    , b []
                                        [ text ( String.fromInt bid ) ]
                                    ]
                                , p []
                                    [ text "With "
                                    , text ( String.fromInt player1Count )
                                    , text " "
                                    , text message
                                    , text " one player receives "
                                    , b []
                                        [ text ( String.fromInt player1 ) ]
                                    ]
                                , p []
                                    [ text "With "
                                    , text ( String.fromInt player2Count )
                                    , text " "
                                    , text message
                                    , text " the other player receives "
                                    , b []
                                        [ text ( String.fromInt player2 ) ]
                                    ]
                                ]
            ]


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }

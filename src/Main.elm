module Main exposing (..)

import Browser
import Html exposing (Html, text, div, ul, li, table, thead, tbody, th, td, nav, h1, h2, p, img, button, b, a, span, em, i, br)
import Html.Attributes exposing (src, class, classList, href, style)
import Html.Keyed exposing (node)
import Html.Events exposing (onClick)


---- MODEL ----

type Merger
    = Shipping
    | Rice
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
        Shipping ->
            10
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

iconUrl : Merger -> String
iconUrl merger =
    case merger of
        Shipping ->
            "%PUBLIC_URL%/images/shipping.png"
        Rice ->
            "%PUBLIC_URL%/images/rice.png"
        Spice ->
            "%PUBLIC_URL%/images/spice.png"
        RiceAndSpice ->
            "%PUBLIC_URL%/images/ricespice.png"
        SiapFaji ->
            "%PUBLIC_URL%/images/siapfaji.png"
        Rubber ->
            "%PUBLIC_URL%/images/rubber.png"
        Oil ->
            "%PUBLIC_URL%/images/oil.png"

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
        [ nav []
            [ div
                [ class "container" ]
                [ div [ class "nav-wrapper" ]
                    [ div
                        [ class "brand-logo hide-on-small-only"
                        , onClick GoHome
                        ]
                        [ text "Indonesia Merger Manager" ]
                    , div
                        [ class "brand-logo hide-on-med-and-up"
                        , onClick GoHome
                        ]
                        [ text "IndoMM" ]
                    , ul
                        [ class "right" ]
                        (
                            if model /= Welcome then
                                [ li []
                                    [ a
                                        [ onClick GoHome ]
                                        [ icon "replay" "right" ]
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
    div
        []
        [ node "div"
            [ class "merger--list" ]
            [
                ( "merger-item-Shipping"
                , a
                    [ onClick ( SelectMerger Shipping )
                    , class "merger-item--container z-depth-1"
                    ]
                    [ img
                        [ src ( iconUrl Shipping )
                        , class "merger-type--icon"
                        ]
                        []
                    , p
                        [ class "merger-item--description" ]
                        [ text "Shipping" ]
                    ]
                )
            ,
                ( "merger-item-Rice"
                , a
                    [ onClick ( SelectMerger Rice )
                    , class "merger-item--container z-depth-1"
                    ]
                    [ img
                        [ src ( iconUrl Rice )
                        , class "merger-type--icon"
                        ]
                        []
                    , p
                        [ class "merger-item--description" ]
                        [ text "Rice" ]
                    ]
                )
            ,
                ( "merger-item-Spice"
                , a
                    [ onClick ( SelectMerger Spice )
                    , class "merger-item--container z-depth-1"
                    ]
                    [ img
                        [ src ( iconUrl Spice )
                        , class "merger-type--icon"
                        ]
                        []
                    , p
                        [ class "merger-item--description" ]
                        [ text "Spice" ]
                    ]
                )
            ,
                ( "merger-item-RiceAndSpice"
                , a
                    [ onClick ( SelectMerger RiceAndSpice )
                    , class "merger-item--container z-depth-1"
                    ]
                    [ img
                        [ src ( iconUrl RiceAndSpice )
                        , class "merger-type--icon"
                        ]
                        []
                    , p
                        [ class "merger-item--description" ]
                        [ text "Rice / Spice"
                        , br [] []
                        , text "(Siap Faji)"
                        ]
                    ]
                )
            ,
                ( "merger-item-SiapFaji"
                , a
                    [ onClick ( SelectMerger SiapFaji )
                    , class "merger-item--container z-depth-1"
                    ]
                    [ img
                        [ src ( iconUrl SiapFaji )
                        , class "merger-type--icon"
                        ]
                        []
                    , p
                        [ class "merger-item--description" ]
                        [ text "Siap Faji" ]
                    ]
                )
            ,
                ( "merger-item-Rubber"
                , a
                    [ onClick ( SelectMerger Rubber )
                    , class "merger-item--container z-depth-1"
                    ]
                    [ img
                        [ src ( iconUrl Rubber )
                        , class "merger-type--icon"
                        ]
                        []
                    , p
                        [ class "merger-item--description" ]
                        [ text "Rubber" ]
                    ]
                )
            ,
                ( "merger-item-Oil"
                , a
                    [ onClick ( SelectMerger Oil )
                    , class "merger-item--container z-depth-1"
                    ]
                    [ img
                        [ src ( iconUrl Oil )
                        , class "merger-type--icon"
                        ]
                        []
                    , p
                        [ class "merger-item--description" ]
                        [ text "Oil" ]
                    ]
                )
            ]
        ]

companySizeButton : Merger -> Int -> ( String, Html Msg )
companySizeButton merger size =
    ( "company-size--" ++ ( String.fromInt size )
    , a
        [ onClick ( SelectCount merger ( Count size ) )
        , class "collection-item"
        ]
        [ text ( String.fromInt size ) ]
    )

companySize : Merger -> Html Msg
companySize merger =
    let
        sizeSelection = companySizeButton merger
    in
        div []
            [ node "div"
                [ class "collection with-header collection-links" ]
                (
                    [
                        ( "company-size--header"
                        , div
                            [ class "collection-header" ]
                            [ h2 [] [ text "Merged company size" ] ]
                        )
                    ] ++ List.map
                        sizeSelection
                        ( List.range 1 25 )
                )
            ]

costTable : Merger -> Count -> Html Msg
costTable merger ( Count count ) =
    let
        pricePerItem = minPrice merger
        initial = count * pricePerItem
    in
        div []
            [ node "div"
                [ class "collection with-header collection-links" ]
                (
                    [
                        ( "bid--header"
                        , div
                            [ class "collection-header" ]
                            [ h2 [] [ text "Final bid" ] ]
                        )
                    ] ++ List.map
                        (\i ->
                            let
                                bid = initial + (i * count)
                            in
                                ( "bid--" ++ ( String.fromInt i )
                                , a
                                    [ onClick ( SelectBid merger ( Count count ) ( Bid bid ) )
                                    , class "collection-item"
                                    ]
                                    [ em [] [ text "Rp " ]
                                    , b []
                                        [ text ( String.fromInt bid ) ]
                                    , span
                                        [ class "grey-text" ]
                                        [ text " : "
                                        , text ( String.fromInt ( bid // count ) )
                                        , text " × "
                                        , text ( String.fromInt count )
                                        , text ""
                                        ]
                                    ]
                                )
                        )
                        ( List.range 0 150 )
                )
            ]

companySplit : Merger -> Count -> Bid -> Html Msg
companySplit merger ( Count count ) bid =
    div []
        [ div [ class "collection with-header collection-links" ]
            (
                [ div
                    [ class "collection-header" ]
                    [ h2 []
                        [ text "Calculate payments" ]
                    ]
                , a
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
                Shipping ->
                    "Shipping"
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
            ( case split of
                SingleCompany ->
                    [ card "col s12"
                        [ p
                            [ class "payment-total--text" ]
                            [ icon "swap_horiz" "large left"
                            , em [] [ text "Rp " ]
                            , b []
                                [ text ( String.fromInt bid ) ]
                            ]
                        ]
                    , card "col s12"
                        [ p []
                            [ text "Single owner receives the full amount" ]
                        ]
                    ]
                Split player1Count player2Count ->
                    let
                        player1 = (bid // count) * player1Count
                        player2 = (bid // count) * player2Count
                    in
                        [ card "col s12"
                            [ p
                                [ class "payment-total--text" ]
                                [ icon "swap_horiz" "large left hide-on-small-only"
                                , em [] [ text "Rp " ]
                                , b []
                                    [ text ( String.fromInt bid ) ]
                                ]
                            ]
                        , card "col s12 m6"
                            [ p
                                [ class "payment-split--text" ]
                                [ icon "person_add" "medium right"
                                , em [] [ text "Rp " ]
                                , b [] [ text ( String.fromInt player1 ) ]
                                ]
                            , p
                                [ class "payment-split--text" ]
                                [ img
                                    [ src ( iconUrl merger )
                                    , class "payment-merger--icon z-depth-1"
                                    ]
                                    []
                                , text " × "
                                , text ( String.fromInt player1Count )
                                ]
                            ]
                        , card "col s12 m6"
                            [ p
                                [ class "payment-split--text" ]
                                [ icon "person_add" "medium right"
                                , em [] [ text "Rp " ]
                                , b [] [ text ( String.fromInt player2 ) ]
                                ]
                            , p
                                [ class "payment-split--text" ]
                                [ img
                                    [ src ( iconUrl merger )
                                    , class "payment-merger--icon z-depth-1"
                                    ]
                                    []
                                , text " × "
                                , text ( String.fromInt player2Count )
                                ]
                            ]
                        ]
            )

card : String -> List ( Html Msg ) -> Html Msg
card cssClass contents =
    div
        [ class cssClass ]
        [ div
            [ class "card" ]
            [ div
                [ class "card-content" ]
                contents
            ]
        ]

icon : String -> String -> Html msg
icon iconName cssClass =
    i
        [ class ( "material-icons " ++ cssClass ) ]
        [ text iconName ]

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }

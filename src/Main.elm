module Main exposing (..)

import Browser
import Html exposing (Html, nav, div, text, ul, li, dl, dt, dd, h2, p, img, b, a, span, em, i, br, strong)
import Html.Attributes exposing (src, class, href)
import Html.Keyed exposing (node)
import Html.Events exposing (onClick)
import Ports exposing (scrollTop)


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
    | AlterCostTable Merger Count Bid Split

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

mergerName : Merger -> String
mergerName merger =
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

mergerClassName : Merger -> String
mergerClassName merger =
    case merger of
        Shipping ->
            "shipping"
        Rice ->
            "rice"
        Spice ->
            "spice"
        RiceAndSpice ->
            "rice-spice"
        SiapFaji ->
            "siap-faji"
        Rubber ->
            "rubber"
        Oil ->
            "oil"

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
    | ChangeBid Merger Count Bid Split
    | UpdateBid Merger Count Split Bid
    | SelectSplit Merger Count Bid Split
    | GoHome


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        SelectMerger merger ->
            ( CompanySize merger
            , scrollTop ()
            )
        SelectCount merger count ->
            ( CostTable merger count
            , scrollTop ()
            )
        SelectBid merger count bid ->
            ( CompanySplit merger count bid
            , scrollTop ()
            )
        ChangeBid merger count bid split ->
            ( AlterCostTable merger count bid split
            , scrollTop ()
            )
        UpdateBid merger count split bid ->
            ( Payments merger count bid split
            , scrollTop ()
            )
        SelectSplit merger count bid split ->
            ( Payments merger count bid split
            , scrollTop ()
            )
        GoHome ->
            ( Welcome
            , scrollTop ()
            )





---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ nav []
            [ div
                [ class "container" ]
                [ div [ class "" ]
                    [ ul
                        [ class "right" ]
                        (
                            if model /= Welcome then
                                [ li []
                                    [ a
                                        [ onClick GoHome ]
                                        [ icon "home" "right" ]
                                    ]
                                ]
                            else
                                []
                        )
                    , div
                        [ class "page-heading left"
                        , onClick GoHome
                        ]
                        [ text "Indoneasier" ]
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
                                costTable merger count ( SelectBid merger count )
                            AlterCostTable merger count prevBid split ->
                                costTable merger count ( UpdateBid merger count split )
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
        [ card "col s12"
            [ p []
                [ text "Indoneasier is an interactive player-aid for the board game "
                , a
                    [ href "https://boardgamegeek.com/boardgame/19777/indonesia" ]
                    [ text "Indonesia" ]
                , text "."
                ]
            ]
        , card "col s12"
            [ h2 []
                [ text "Perform merger" ]
            , node "div"
                [ class "merger--list" ]
                [ mergerButton Shipping
                , mergerButton Rice
                , mergerButton Spice
                , mergerButton RiceAndSpice
                , mergerButton SiapFaji
                , mergerButton Rubber
                , mergerButton Oil
                ]
            ]
        , card "col s12"
            [ h2 []
                [ text "Game phases" ]
            , dl
                [ class "dl__docs" ]
                [ dt [] [ text "1. New era" ]
                , dd []
                    [ text "Occurs when at most one company type remains, "
                    , text "and at game start."
                    , br [] []
                    , text "- remove any remaining companies"
                    , br [] []
                    , text "- players place next era's city"
                    , br [] []
                    , text "- distribute next era's companies"
                    , br [] []
                    , text "Game ends if era "
                    , strong [] [ text "c" ]
                    , text " ends."
                    ]
                , dt [] [ text "2. Turn order" ]
                , dd []
                    [ text "One bid per player, in previous turn order. "
                    , text "New turn order is bid amount ranked, tied players maintain their respective order."
                    , br [] []
                    , text "Player bids are multiplied before ranking according to their "
                    , strong [] [ text "turn order bid"]
                    , text " R&D level."
                    , br [] []
                    , icon "info" "tiny"
                    , text " Pay bids into player banks."
                    ]
                , dt [] [ text "3. Mergers" ]
                , dd []
                    [ text "In turn order, players may announce a merger between any 2 companies "
                    , text "until all players are unwilling or unable to announce another."
                    , br [] []
                    , text "Number of deeds in an announced merger is limited by player's "
                    , strong [] [ text "merger"]
                    , text " R&D level."
                    , br [] []
                    , text "Announcing player must be able to hold the resulting company "
                    , text "(i.e. owns one of the companies or has a free slot)."
                    , br [] []
                    , text "All players that could hold the resulting company may bid."
                    , br [] []
                    , icon "info" "tiny"
                    , text " Merging rice & spice into siap faji cannot be done in era "
                    , strong [] [ text "a" ]
                    , text "."
                    , br [] []
                    , icon "info" "tiny"
                    , text " After creating siap faji from rice & spice remove half (round up) land areas."
                    ]
                , dt [] [ text "4. Acquisitions" ]
                , dd []
                    [ text "In turn order, players may acquire an available company "
                    , text "until all companies are taken, "
                    , text "or players are unwilling or unable to take another company."
                    , br [] []
                    , text "Players are limited by their "
                    , strong [] [ text "slots"]
                    , text " R&D level."
                    ]
                , dt [] [ text "5. Research & Development" ]
                , dd []
                    [ text "In turn order, players move marker one step forward on a single track."
                    , br [] []
                    , icon "info" "tiny"
                    , text " May upgrade another player's hull size."
                    ]
                , dt [] [ text "6. Operations" ]
                , dd []
                    [ text "In turn order, players operate one of their companies "
                    , text "until all companies have operated."
                    , br [] []
                    , text "Goods companies:"
                    , br [] []
                    , text "- pay shipping costs"
                    , br [] []
                    , text "- must ship as much as possible"
                    , br [] []
                    , text "- expand for free if all goods sold, or pay to expand"
                    , br [] []
                    , text "Shipping companies:"
                    , br [] []
                    , text "- may expand for free, up to company's era capacity"
                    , br [] []
                    , text "Expansions are limited by players' "
                    , strong [] [ text "expansion"]
                    , text " R&D level."
                    , br [] []
                    , icon "info" "tiny"
                    , text " Double earnings in the final round."
                    ]
                , dt [] [ text "7. City growth" ]
                , dd []
                    [ text "Cities grow if they were full of all goods types available in this era." ]
                ]
            ]
        , card "col m6 s12"
            [ h2 []
                [ text "R&D tracks" ]
            , dl
                [ class "dl__docs" ]
                [ dt [] [ text "Slots" ]
                , dd []
                    [ text "Number of companies the player may own at any one time. "
                    , br [] []
                    , icon "info" "tiny"
                    , text " Merged companies take up only one slot."
                    ]
                , dt [] [ text "Mergers" ]
                , dd []
                    [ text "Limits the size of company for which the player can announce a merger."
                    ]
                , dt [] [ text "Hull player" ]
                , dd []
                    [ text "The number of goods that can be carried by ships belonging to shipping companies owned by the player. "
                    ]
                , dt [] [ text "Expansion" ]
                , dd []
                    [ text "The maximum number of expansions that companies owned by the player may perform each time they operate."
                    ]
                , dt [] [ text "Turn order bid" ]
                , dd []
                    [ text "Multiplies turn order bids made by the player for the purposes of ranking, as indicated."
                    ]
                ]
            ]
        , card "col m6 s12"
            [ h2 []
                [ text "Goods values" ]
            , dl
                []
                [ dt [] [ text "Shipping" ]
                , dd []
                    [ em [] [ text "Rp " ]
                    , text "5 / ship"
                    ]
                , dt [] [ text "Rice" ]
                , dd []
                    [ em [] [ text "Rp " ]
                    , text "20"
                    ]
                , dt [] [ text "Spice" ]
                , dd []
                    [ em [] [ text "Rp " ]
                    , text "25"
                    ]
                , dt [] [ text "SiapFaji" ]
                , dd []
                    [ em [] [ text "Rp " ]
                    , text "35"
                    ]
                , dt [] [ text "Rubber" ]
                , dd []
                    [ em [] [ text "Rp " ]
                    , text "30"
                    ]
                , dt [] [ text "Oil" ]
                , dd []
                    [ em [] [ text "Rp " ]
                    , text "50"
                    ]
                ]
            ]
        ]

mergerButton : Merger -> ( String, Html Msg )
mergerButton merger =
    let
        name =
            if merger == RiceAndSpice then
                "Rice & Spice (Siap faji)"
            else
                mergerName merger
        nameEls =
            [ text name
            , br [] []
            , span
                [ class "grey-text" ]
                [ em []
                    [ text "Rp "
                    ]
                , text <| String.fromInt <| minPrice merger
                ]
            ]
    in
    ( "merger-item-" ++ mergerName merger
    , a
        [ onClick ( SelectMerger merger )
        , class ("merger-item--container z-depth-1 company-type--" ++ mergerClassName merger)
        ]
        [ img
            [ src ( iconUrl merger )
            , class "merger-type--icon"
            ]
            []
        , p
            [ class "merger-item--description" ]
            nameEls
        ]
    )


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

costTable : Merger -> Count -> ( Bid -> Msg ) -> Html Msg
costTable merger ( Count count ) clickMsg =
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
                            [ h2 [] [ text "Bid amount" ] ]
                        )
                    ] ++ List.map
                        (\i ->
                            let
                                bid = initial + (i * count)
                            in
                                ( "bid--" ++ ( String.fromInt i )
                                , a
                                    [ onClick ( clickMsg ( Bid bid ) )
                                    , class "collection-item"
                                    ]
                                    [ em [] [ text "Rp " ]
                                    , b []
                                        [ text ( String.fromInt bid ) ]
                                    , span
                                        [ class "grey-text" ]
                                        [ text " : "
                                        , text ( String.fromInt count )
                                        , text " × "
                                        , text ( String.fromInt ( bid // count ) )
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
    div []
        ( case split of
            SingleCompany ->
                [ card "col s12"
                    [ p
                        [ class "payment-total--text hide-on-very-small-400-only" ]
                        [ icon "swap_horiz" "large left" ]
                    , a
                        [ class "payment-total--button waves-effect waves-light btn btn-large blue-grey lighten-3"
                        , onClick ( ChangeBid merger (Count count) (Bid bid) split )
                        ]
                        [ icon "edit" "right large"
                        , em [] [ text "Rp "]
                        , text ( String.fromInt bid )
                        ]
                    ]
                , card "col s12"
                    [ p
                        [ class "payment-split--text" ]
                        [ icon "person_add" "medium right"
                        , em [] [ text "Rp " ]
                        , b [] [ text ( String.fromInt bid ) ]
                        ]
                    , p []
                        [ text "Single owner receives the full bid amount" ]
                    ]
                ]
            Split player1Count player2Count ->
                let
                    player1 = (bid // count) * player1Count
                    player2 = (bid // count) * player2Count
                in
                    [ card "col s12"
                        [ p
                            [ class "payment-total--text hide-on-very-small-400-only" ]
                            [ icon "swap_horiz" "large left" ]
                        , a
                            [ class "payment-total--button waves-effect waves-light btn btn-large blue-grey lighten-3"
                            , onClick ( ChangeBid merger (Count count) (Bid bid) split )
                            ]
                            [ icon "edit" "right large"
                            , em [] [ text "Rp "]
                            , text ( String.fromInt bid )
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

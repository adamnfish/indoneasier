module Main exposing (..)

import Browser
import Html exposing (Html, a, b, br, dd, div, dl, dt, em, footer, h2, i, img, li, main_, nav, p, span, strong, text, ul)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onClick)
import Html.Keyed exposing (node)
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


type Count
    = Count Int


type Bid
    = Bid Int


type Split
    = SingleCompany
    | Split Int Int


type alias Assets =
    { shipping : String
    , rice : String
    , spice : String
    , ricespice : String
    , siapfaji : String
    , rubber : String
    , oil : String
    }


type alias Flags =
    { assets : Assets
    }


type Lifecycle
    = Welcome
    | CompanySize Merger
    | CostTable Merger Count
    | CompanySplit Merger Count Bid
    | Payments Merger Count Bid Split
    | AlterCostTable Merger Count Bid Split


type alias Model =
    { lifecycle : Lifecycle
    , assets : Assets
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { lifecycle = Welcome
      , assets = flags.assets
      }
    , Cmd.none
    )


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


iconUrl : Assets -> Merger -> String
iconUrl assets merger =
    case merger of
        Shipping ->
            assets.shipping

        Rice ->
            assets.rice

        Spice ->
            assets.spice

        RiceAndSpice ->
            assets.ricespice

        SiapFaji ->
            assets.siapfaji

        Rubber ->
            assets.rubber

        Oil ->
            assets.oil



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
update msg model =
    case msg of
        SelectMerger merger ->
            ( { model | lifecycle = CompanySize merger }
            , scrollTop ()
            )

        SelectCount merger count ->
            ( { model | lifecycle = CostTable merger count }
            , scrollTop ()
            )

        SelectBid merger count bid ->
            ( { model | lifecycle = CompanySplit merger count bid }
            , scrollTop ()
            )

        ChangeBid merger count bid split ->
            ( { model | lifecycle = AlterCostTable merger count bid split }
            , scrollTop ()
            )

        UpdateBid merger count split bid ->
            ( { model | lifecycle = Payments merger count bid split }
            , scrollTop ()
            )

        SelectSplit merger count bid split ->
            ( { model | lifecycle = Payments merger count bid split }
            , scrollTop ()
            )

        GoHome ->
            ( { model | lifecycle = Welcome }
            , scrollTop ()
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ nav []
            [ div
                [ class "nav-title"
                , onClick GoHome
                ]
                [ text "Indoneasier" ]
            , div [ class "nav-bg" ] []
            ]
        , div [ class "welcome-description" ]
            [ text "an interactive player-aid for the board game "
            , a
                [ href "https://boardgamegeek.com/boardgame/19777/indonesia" ]
                [ text "Indonesia" ]
            ]
        , main_
            [ class "container" ]
            [ case model.lifecycle of
                Welcome ->
                    welcome model.assets

                CompanySize merger ->
                    companySize merger

                CostTable merger count ->
                    costTable merger count (SelectBid merger count)

                AlterCostTable merger count prevBid split ->
                    costTable merger count (UpdateBid merger count split)

                CompanySplit merger count bid ->
                    companySplit merger count bid

                Payments merger count bid split ->
                    payments model.assets merger count bid split
            ]
        , div
            [ class "welcome-description" ]
            [ a [ href "https://boardgamegeek.com/user/adamnfish" ] [ text "adamnfish" ]
            , text " | "
            , a [ href "https://github.com/adamnfish/indoneasier" ] [ text "source code" ]
            ]
        , footer
            []
            []
        ]


welcome : Assets -> Html Msg
welcome assets =
    div
        []
        [ card "Perform merger"
            [ node "div"
                [ class "merger--list" ]
                [ mergerButton assets Shipping
                , mergerButton assets Rice
                , mergerButton assets Spice
                , mergerButton assets RiceAndSpice
                , mergerButton assets SiapFaji
                , mergerButton assets Rubber
                , mergerButton assets Oil
                ]
            ]
        , card "Game phases"
            [ dl
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
                    , text "Game ends if "
                    , strong [] [ text "era c" ]
                    , text " ends."
                    ]
                , dt [] [ text "2. Turn order" ]
                , dd []
                    [ text "One bid per player, in previous turn order. "
                    , text "New turn order is bid amount ranked, tied players maintain their respective order."
                    , br [] []
                    , text "Player bids are multiplied before ranking according to their "
                    , strong [] [ text "turn order bid" ]
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
                    , strong [] [ text "merger" ]
                    , text " R&D level."
                    , br [] []
                    , text "Announcing player must be able to hold the resulting company "
                    , text "(i.e. owns one of the companies or has a free slot)."
                    , br [] []
                    , text "All players that could hold the resulting company may bid."
                    , br [] []
                    , icon "info" "tiny"
                    , text " Merging rice & spice into siap faji cannot be done in "
                    , strong [] [ text "era a" ]
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
                    , strong [] [ text "slots" ]
                    , text " R&D level."
                    , br [] []
                    , icon "info" "tiny"
                    , text " Merged companies of any size take up only one slot."
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
                    , text "- ship goods to cities with capacity"
                    , br [] []
                    , text "- each good is sold on a chain of ships from a single company"
                    , br [] []
                    , text "- pay shipping costs to owners"
                    , br [] []
                    , text "- must ship as much as possible"
                    , br [] []
                    , text "- must expand for free if all goods sold, or may pay to expand"
                    , br [] []
                    , text "Shipping companies:"
                    , br [] []
                    , text "- may expand for free, up to company's era capacity"
                    , br [] []
                    , text "Expansions are limited by players' "
                    , strong [] [ text "expansion" ]
                    , text " R&D level."
                    , br [] []
                    , icon "info" "tiny"
                    , text " Double earnings in the final round."
                    ]
                , dt [] [ text "7. City growth" ]
                , dd []
                    [ text "Cities grow if they were full of all available goods types." ]
                ]
            ]
        , card "R&D tracks"
            [ dl
                [ class "dl__docs" ]
                [ dt [] [ text "Slots" ]
                , dd []
                    [ text "Number of companies the player may own at any one time. "
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
        , card "Goods values"
            [ dl
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
                    , text "40"
                    ]
                ]
            ]
        ]


mergerButton : Assets -> Merger -> ( String, Html Msg )
mergerButton assets merger =
    let
        name =
            if merger == RiceAndSpice then
                span []
                    [ span
                        [ class "nowrap" ]
                        [ text "Rice & Spice" ]
                    , text " "
                    , span
                        [ class "nowrap" ]
                        [ text "(Siap faji)" ]
                    ]

            else
                text <| mergerName merger

        nameEls =
            [ name
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
        [ onClick (SelectMerger merger)
        , class ("merger-item--container z-depth-1 company-type--" ++ mergerClassName merger)
        ]
        [ img
            [ src (iconUrl assets merger)
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
    ( "company-size--" ++ String.fromInt size
    , a
        [ onClick (SelectCount merger (Count size))
        , class "collection-item"
        ]
        [ text (String.fromInt size) ]
    )


companySize : Merger -> Html Msg
companySize merger =
    let
        sizeSelection =
            companySizeButton merger
    in
    div []
        [ node "div"
            [ class "collection with-header collection-links" ]
            ([ ( "company-size--header"
               , div
                    [ class "collection-header" ]
                    [ h2 [] [ text "Merged company size" ] ]
               )
             ]
                ++ List.map
                    sizeSelection
                    (List.range 1 25)
            )
        ]


costTable : Merger -> Count -> (Bid -> Msg) -> Html Msg
costTable merger (Count count) clickMsg =
    let
        pricePerItem =
            minPrice merger

        initial =
            count * pricePerItem
    in
    div []
        [ node "div"
            [ class "collection with-header collection-links" ]
            ([ ( "bid--header"
               , div
                    [ class "collection-header" ]
                    [ h2 [] [ text "Bid amount" ] ]
               )
             ]
                ++ List.map
                    (\i ->
                        let
                            bid =
                                initial + (i * count)
                        in
                        ( "bid--" ++ String.fromInt i
                        , a
                            [ onClick (clickMsg (Bid bid))
                            , class "collection-item"
                            ]
                            [ em [] [ text "Rp " ]
                            , b []
                                [ text (String.fromInt bid) ]
                            , span
                                [ class "grey-text" ]
                                [ text " : "
                                , text (String.fromInt count)
                                , text " × "
                                , text (String.fromInt (bid // count))
                                , text ""
                                ]
                            ]
                        )
                    )
                    (List.range 0 150)
            )
        ]


companySplit : Merger -> Count -> Bid -> Html Msg
companySplit merger (Count count) bid =
    div []
        [ div [ class "collection with-header collection-links" ]
            ([ div
                [ class "collection-header" ]
                [ h2 []
                    [ text "Calculate payments" ]
                ]
             , a
                [ onClick (SelectSplit merger (Count count) bid SingleCompany)
                , class "collection-item"
                ]
                [ text "Single owner" ]
             ]
                ++ List.map
                    (\i ->
                        a
                            [ onClick (SelectSplit merger (Count count) bid (Split i (count - i)))
                            , class "collection-item"
                            ]
                            [ text (String.fromInt i)
                            , text " / "
                            , text (String.fromInt (count - i))
                            ]
                    )
                    (List.range 1 (count // 2))
            )
        ]


payments : Assets -> Merger -> Count -> Bid -> Split -> Html Msg
payments assets merger (Count count) (Bid bid) split =
    div []
        (case split of
            SingleCompany ->
                [ card "TODO"
                    [ p
                        [ class "payment-total--text hide-on-very-small-400-only" ]
                        [ icon "swap_horiz" "large left" ]
                    , a
                        [ class "payment-total--button waves-effect waves-light btn btn-large blue-grey lighten-3"
                        , onClick (ChangeBid merger (Count count) (Bid bid) split)
                        ]
                        [ icon "edit" "right large"
                        , em [] [ text "Rp " ]
                        , text (String.fromInt bid)
                        ]
                    ]
                , card "TODO"
                    [ p
                        [ class "payment-split--text" ]
                        [ icon "person_add" "medium right"
                        , em [] [ text "Rp " ]
                        , b [] [ text (String.fromInt bid) ]
                        ]
                    , p []
                        [ text "Single owner receives the full bid amount" ]
                    ]
                ]

            Split player1Count player2Count ->
                let
                    player1 =
                        (bid // count) * player1Count

                    player2 =
                        (bid // count) * player2Count
                in
                [ card "TODO"
                    [ p
                        [ class "payment-total--text hide-on-very-small-400-only" ]
                        [ icon "swap_horiz" "large left" ]
                    , a
                        [ class "payment-total--button waves-effect waves-light btn btn-large blue-grey lighten-3"
                        , onClick (ChangeBid merger (Count count) (Bid bid) split)
                        ]
                        [ icon "edit" "right large"
                        , em [] [ text "Rp " ]
                        , text (String.fromInt bid)
                        ]
                    ]
                , card "TODO"
                    [ p
                        [ class "payment-split--text" ]
                        [ icon "person_add" "medium right"
                        , em [] [ text "Rp " ]
                        , b [] [ text (String.fromInt player1) ]
                        ]
                    , p
                        [ class "payment-split--text" ]
                        [ img
                            [ src (iconUrl assets merger)
                            , class "payment-merger--icon z-depth-1"
                            ]
                            []
                        , text " × "
                        , text (String.fromInt player1Count)
                        ]
                    ]
                , card "TODO"
                    [ p
                        [ class "payment-split--text" ]
                        [ icon "person_add" "medium right"
                        , em [] [ text "Rp " ]
                        , b [] [ text (String.fromInt player2) ]
                        ]
                    , p
                        [ class "payment-split--text" ]
                        [ img
                            [ src (iconUrl assets merger)
                            , class "payment-merger--icon z-depth-1"
                            ]
                            []
                        , text " × "
                        , text (String.fromInt player2Count)
                        ]
                    ]
                ]
        )


card : String -> List (Html Msg) -> Html Msg
card heading contents =
    div
        []
        [ div
            [ class "card" ]
            [ div
                [ class "card-title" ]
                [ h2 [] [ text heading ] ]
            , div
                [ class "card-content" ]
                contents
            ]
        ]


icon : String -> String -> Html msg
icon iconName cssClass =
    i
        [ class ("material-icons " ++ cssClass) ]
        [ text iconName ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

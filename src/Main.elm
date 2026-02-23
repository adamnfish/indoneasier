module Main exposing (..)

import Browser
import Html exposing (Html, a, button, dd, div, dl, dt, em, footer, h2, img, li, main_, nav, p, span, strong, text, ul)
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
                [ text "Indon"
                , strong [] [ text "easier" ]
                ]
            , div [ class "nav-bg" ] []
            ]
        , div [ class "welcome-description" ]
            [ text "an interactive player-aid for the board game "
            , a
                [ href "https://boardgamegeek.com/boardgame/19777/indonesia" ]
                [ text "Indonesia" ]
            ]
        , main_
            []
            [ case model.lifecycle of
                Welcome ->
                    welcome model.assets

                CompanySize merger ->
                    companySize merger

                CostTable merger count ->
                    costTable merger count (SelectBid merger count)

                AlterCostTable merger count _ split ->
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
        [ class "welcome" ]
        [ p [ class "merger--hint" ] [ text "Select a company type to calculate a merger" ]
        , node "div"
            [ class "merger--list" ]
            [ mergerButton assets Shipping
            , mergerButton assets Rice
            , mergerButton assets Spice
            , mergerButton assets RiceAndSpice
            , mergerButton assets SiapFaji
            , mergerButton assets Rubber
            , mergerButton assets Oil
            ]
        , div [ class "docs-layout" ]
            [ div [ class "doc-card" ]
                [ div [ class "doc-card-title" ] [ h2 [] [ text "Game phases" ] ]
                , div [ class "doc-card-content" ]
                    [ div [ class "phase" ]
                        [ div [ class "phase-header" ]
                            [ span [ class "phase-number" ] [ text "1" ]
                            , span [ class "phase-name" ] [ text "New era" ]
                            ]
                        , ul [ class "phase-rules" ]
                            [ li [] [ text "Occurs when at most one company type remains, and at game start" ]
                            , li [] [ text "Remove any remaining companies" ]
                            , li [] [ text "Players place next era's city" ]
                            , li [] [ text "Distribute next era's companies" ]
                            , li [] [ text "Game ends if era c ends" ]
                            ]
                        ]
                    , div [ class "phase" ]
                        [ div [ class "phase-header" ]
                            [ span [ class "phase-number" ] [ text "2" ]
                            , span [ class "phase-name" ] [ text "Turn order" ]
                            ]
                        , ul [ class "phase-rules" ]
                            [ li [] [ text "One bid per player, in previous turn order" ]
                            , li [] [ text "New turn order ranked by bid amount; tied players maintain their respective order" ]
                            , li [] [ text "Player bids are multiplied before ranking according to their turn order bid R&D level" ]
                            , li [ class "phase-callout" ] [ text "Pay bids into player banks" ]
                            ]
                        ]
                    , div [ class "phase" ]
                        [ div [ class "phase-header" ]
                            [ span [ class "phase-number" ] [ text "3" ]
                            , span [ class "phase-name" ] [ text "Mergers" ]
                            ]
                        , ul [ class "phase-rules" ]
                            [ li [] [ text "In turn order, players may announce a merger between any 2 companies until all players are unwilling or unable" ]
                            , li [ class "phase-callout" ]
                                [ text "Merging rice & spice into siap faji cannot be done in "
                                , strong [] [ text "era a" ]
                                ]
                            , li [] [ text "Size of the announced merger is limited by the announcing player's merger R&D level" ]
                            , li [] [ text "Announcing player must be able to hold the resulting company" ]
                            , li [] [ text "All players that could hold the resulting company may bid" ]
                            , li [ class "phase-callout" ] [ text "After creating siap faji from rice & spice, remove half (round up) land areas" ]
                            ]
                        ]
                    , div [ class "phase" ]
                        [ div [ class "phase-header" ]
                            [ span [ class "phase-number" ] [ text "4" ]
                            , span [ class "phase-name" ] [ text "Acquisitions" ]
                            ]
                        , ul [ class "phase-rules" ]
                            [ li [] [ text "In turn order, players may acquire an available company until all are taken or players are unwilling or unable" ]
                            , li [] [ text "Players are limited by their slots R&D level" ]
                            , li [ class "phase-callout" ] [ text "Merged companies of any size take up only one slot" ]
                            ]
                        ]
                    , div [ class "phase" ]
                        [ div [ class "phase-header" ]
                            [ span [ class "phase-number" ] [ text "5" ]
                            , span [ class "phase-name" ] [ text "Research & Development" ]
                            ]
                        , ul [ class "phase-rules" ]
                            [ li [] [ text "In turn order, players move their marker one step forward on a single track" ]
                            , li [ class "phase-callout" ] [ text "May upgrade another player's hull size" ]
                            ]
                        ]
                    , div [ class "phase" ]
                        [ div [ class "phase-header" ]
                            [ span [ class "phase-number" ] [ text "6" ]
                            , span [ class "phase-name" ] [ text "Operations" ]
                            ]
                        , ul [ class "phase-rules" ]
                            [ li [] [ text "In turn order, players operate one of their companies until all have operated" ]
                            , li [] [ strong [] [ text "Goods: " ], text "ship goods to cities with capacity; each good sold on a chain of ships from a single company; pay shipping costs to owners; must ship as much as possible; must expand for free if all goods sold, or may pay to expand" ]
                            , li [] [ strong [] [ text "Shipping: " ], text "may expand for free, up to company's era capacity" ]
                            , li [] [ text "Expansions limited by players' expansion R&D level" ]
                            , li [ class "phase-callout" ] [ text "Double earnings in the final round" ]
                            ]
                        ]
                    , div [ class "phase phase--last" ]
                        [ div [ class "phase-header" ]
                            [ span [ class "phase-number" ] [ text "7" ]
                            , span [ class "phase-name" ] [ text "City growth" ]
                            ]
                        , ul [ class "phase-rules" ]
                            [ li [] [ text "Cities grow if they were full of all available goods types" ]
                            ]
                        ]
                    ]
                ]
            , div [ class "docs-sidebar" ]
                [ div [ class "doc-card" ]
                    [ div [ class "doc-card-title" ] [ h2 [] [ text "R&D tracks" ] ]
                    , div [ class "doc-card-content doc-card-content--padded" ]
                        [ dl
                            [ class "dl__docs" ]
                            [ div [ class "dl__docs-item" ]
                                [ span [ class "dl__docs-disc" ] []
                                , dt [] [ text "Slots" ]
                                , dd [] [ text "Number of companies the player may own at any one time." ]
                                ]
                            , div [ class "dl__docs-item" ]
                                [ span [ class "dl__docs-disc" ] []
                                , dt [] [ text "Mergers" ]
                                , dd [] [ text "Limits the size of company for which the player can announce a merger." ]
                                ]
                            , div [ class "dl__docs-item" ]
                                [ span [ class "dl__docs-disc" ] []
                                , dt [] [ text "Hull size" ]
                                , dd [] [ text "The number of goods that can be carried by ships belonging to shipping companies owned by the player." ]
                                ]
                            , div [ class "dl__docs-item" ]
                                [ span [ class "dl__docs-disc" ] []
                                , dt [] [ text "Expansion" ]
                                , dd [] [ text "The maximum number of expansions that companies owned by the player may perform each time they operate." ]
                                ]
                            , div [ class "dl__docs-item" ]
                                [ span [ class "dl__docs-disc" ] []
                                , dt [] [ text "Turn order bid" ]
                                , dd [] [ text "Multiplies turn order bids made by the player for the purposes of ranking, as indicated." ]
                                ]
                            ]
                        ]
                    ]
                , div [ class "doc-card" ]
                    [ div [ class "doc-card-title" ] [ h2 [] [ text "Goods values" ] ]
                    , div [ class "doc-card-content" ]
                        [ div [ class "goods-row" ]
                            [ img [ src assets.shipping, class "goods-icon" ] []
                            , span [ class "goods-name" ] [ text "Shipping" ]
                            , span [ class "goods-price" ] [ em [] [ text "Rp " ], text "5 / ship" ]
                            ]
                        , div [ class "goods-row" ]
                            [ img [ src assets.rice, class "goods-icon" ] []
                            , span [ class "goods-name" ] [ text "Rice" ]
                            , span [ class "goods-price" ] [ em [] [ text "Rp " ], text "20" ]
                            ]
                        , div [ class "goods-row" ]
                            [ img [ src assets.spice, class "goods-icon" ] []
                            , span [ class "goods-name" ] [ text "Spice" ]
                            , span [ class "goods-price" ] [ em [] [ text "Rp " ], text "25" ]
                            ]
                        , div [ class "goods-row" ]
                            [ img [ src assets.siapfaji, class "goods-icon" ] []
                            , span [ class "goods-name" ] [ text "Siap Faji" ]
                            , span [ class "goods-price" ] [ em [] [ text "Rp " ], text "35" ]
                            ]
                        , div [ class "goods-row" ]
                            [ img [ src assets.rubber, class "goods-icon" ] []
                            , span [ class "goods-name" ] [ text "Rubber" ]
                            , span [ class "goods-price" ] [ em [] [ text "Rp " ], text "30" ]
                            ]
                        , div [ class "goods-row" ]
                            [ img [ src assets.oil, class "goods-icon" ] []
                            , span [ class "goods-name" ] [ text "Oil" ]
                            , span [ class "goods-price" ] [ em [] [ text "Rp " ], text "40" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


mergerButton : Assets -> Merger -> ( String, Html Msg )
mergerButton assets merger =
    let
        name =
            span [ class "merger-item--name" ] [ text <| mergerName merger ]
    in
    ( "merger-item-" ++ mergerName merger
    , button
        [ onClick (SelectMerger merger)
        , class ("merger-item--container company-type--" ++ mergerClassName merger)
        ]
        [ img
            [ src (iconUrl assets merger)
            , class "merger-type--icon"
            ]
            []
        , name
        , span
            [ class "merger-item--price" ]
            [ em [] [ text "Rp " ]
            , text <| String.fromInt <| minPrice merger
            ]
        ]
    )


companySizeButton : Merger -> Int -> ( String, Html Msg )
companySizeButton merger size =
    ( "company-size--" ++ String.fromInt size
    , button
        [ onClick (SelectCount merger (Count size))
        , class "size-item--button"
        ]
        [ span [ class "size-item--number" ] [ text (String.fromInt size) ]
        ]
    )


companySize : Merger -> Html Msg
companySize merger =
    let
        sizeSelection =
            companySizeButton merger
    in
    div [ class "screen" ]
        [ p [ class "merger--hint" ] [ text "Select the size of the merged company" ]
        , node "div"
            [ class "size--list" ]
            (List.map sizeSelection (List.range 1 35))
        ]


costTable : Merger -> Count -> (Bid -> Msg) -> Html Msg
costTable merger (Count count) clickMsg =
    let
        pricePerItem =
            minPrice merger

        initial =
            count * pricePerItem
    in
    div [ class "screen" ]
        [ p [ class "merger--hint" ] [ text "Select the winning bid amount" ]
        , node "div"
            [ class "bid--list" ]
            (List.map
                (\i ->
                    let
                        bid =
                            initial + (i * count)
                    in
                    ( "bid--" ++ String.fromInt i
                    , button
                        [ onClick (clickMsg (Bid bid))
                        , class "bid-item--button"
                        ]
                        [ span [ class "bid-item--total" ]
                            [ em [ class "bid-item--currency" ] [ text "Rp" ]
                            , text (String.fromInt bid)
                            ]
                        , span [ class "bid-item--breakdown" ]
                            [ text (String.fromInt count)
                            , text " × "
                            , em [] [ text "Rp" ]
                            , text (String.fromInt (bid // count))
                            ]
                        ]
                    )
                )
                (List.range 0 150)
            )
        ]


companySplit : Merger -> Count -> Bid -> Html Msg
companySplit merger (Count count) bid =
    div [ class "screen" ]
        [ p [ class "merger--hint" ] [ text "Select the ownership split" ]
        , div [ class "split--list" ]
            ([ button
                [ onClick (SelectSplit merger (Count count) bid SingleCompany)
                , class "split-item--button"
                ]
                [ span [ class "split-item--ratio" ] [ text "Single" ]
                , span [ class "split-item--ratio" ] [ text "owner" ]
                ]
             ]
                ++ List.map
                    (\i ->
                        button
                            [ onClick (SelectSplit merger (Count count) bid (Split i (count - i)))
                            , class "split-item--button"
                            ]
                            [ span [ class "split-item--ratio" ]
                                [ text (String.fromInt i)
                                , span [ class "split-item--divider" ] [ text "/" ]
                                , text (String.fromInt (count - i))
                                ]
                            ]
                    )
                    (List.range 1 (count // 2))
            )
        ]


payments : Assets -> Merger -> Count -> Bid -> Split -> Html Msg
payments assets merger (Count count) (Bid bid) split =
    let
        pricePerDeed =
            bid // count

        mergerIcon =
            img
                [ src (iconUrl assets merger)
                , class "payment-merger--icon"
                ]
                []

        paymentRow amount dealsCount =
            div [ class "payment-row" ]
                [ div [ class "payment-row--breakdown" ]
                    [ mergerIcon
                    , span [ class "payment-row--deeds" ]
                        [ text "× "
                        , text (String.fromInt dealsCount)
                        ]
                    , span [ class "payment-row--per-deed" ]
                        [ text "("
                        , em [] [ text "Rp " ]
                        , text (String.fromInt pricePerDeed)
                        , text " each)"
                        ]
                    ]
                , div [ class "payment-row--amount" ]
                    [ em [ class "payment-row--currency" ] [ text "Rp" ]
                    , text (String.fromInt amount)
                    ]
                ]
    in
    div [ class "screen" ]
        [ button
            [ class "payment-total"
            , onClick (ChangeBid merger (Count count) (Bid bid) split)
            ]
            [ div [ class "payment-total--label" ] [ text "Total bid" ]
            , div [ class "payment-total--amount" ]
                [ em [ class "payment-total--currency" ] [ text "Rp" ]
                , text (String.fromInt bid)
                ]
            , div [ class "payment-total--hint" ] [ text "tap to change" ]
            ]
        , case split of
            SingleCompany ->
                div [ class "payment-rows" ]
                    [ paymentRow bid count ]

            Split player1Count player2Count ->
                div [ class "payment-rows" ]
                    [ paymentRow (pricePerDeed * player1Count) player1Count
                    , paymentRow (pricePerDeed * player2Count) player2Count
                    ]
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

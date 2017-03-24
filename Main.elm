module GrayGoo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time, second)


-- MODEL


type alias Model =
    { gooQuantity : Int
    , lastUpdate : Time
    , replicationTime : Float
    }


initialModel : Model
initialModel =
    Model 1 0 5



-- UPDATE


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | gooQuantity = model.gooQuantity * 2 }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every
            (model.replicationTime * second)
            Tick
        ]



-- VIEW


gooInfo : Int -> String
gooInfo quantity =
    "Nanomachine: " ++ (toString quantity)


viewGoo : Int -> Html Msg
viewGoo quantity =
    p [] [ text (gooInfo quantity) ]


view : Model -> Html Msg
view model =
    div [ class "content" ] [ viewGoo model.gooQuantity ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

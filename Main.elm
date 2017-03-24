module GrayGoo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time exposing (Time, second)


-- MODEL


type alias Model =
    { nanomachineQuantity : Int
    , lastUpdate : Time
    , factory : NanomachineFactory
    , rawMaterials : Int
    , nanomachineCost : Int
    }


type alias NanomachineFactory =
    { buildTime : Int
    }


initialModel : Model
initialModel =
    Model 1 0 initialNanomachineFactory initialRawMaterials initialNonaMachineCost


initialBuildTime =
    5


initialRawMaterials =
    0


initialNonaMachineCost =
    100


initialNanomachineFactory : NanomachineFactory
initialNanomachineFactory =
    NanomachineFactory initialBuildTime



-- UPDATE


type Msg
    = Tick Time
    | ConvertNanomachine


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | rawMaterials = model.rawMaterials + model.nanomachineQuantity * 1 }, Cmd.none )

        ConvertNanomachine ->
            if model.rawMaterials >= model.nanomachineCost then
                ( { model
                    | nanomachineQuantity = model.nanomachineQuantity + 1
                    , rawMaterials = model.rawMaterials - model.nanomachineCost
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every second Tick ]



-- VIEW


nanomachineInfo : Int -> String
nanomachineInfo quantity =
    "Nanomachine: " ++ (toString quantity)


materialInfo : Int -> String
materialInfo quantity =
    "Raw Materials: " ++ (toString quantity)


viewNanomachine : Int -> Html Msg
viewNanomachine quantity =
    p [] [ text (nanomachineInfo quantity) ]


viewMaterials : Int -> Html Msg
viewMaterials quantity =
    p [] [ text (materialInfo quantity) ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewNanomachine model.nanomachineQuantity
        , viewMaterials model.rawMaterials
        , div [] [ button [ onClick ConvertNanomachine ] [ text ("Convert nanomachine (100 raw materials)") ] ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

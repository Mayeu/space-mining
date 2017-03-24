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
    , storageLevel : Int
    , expandStorageCost : Int
    }


type alias NanomachineFactory =
    { buildTime : Int
    }


initialModel : Model
initialModel =
    Model 1 0 initialNanomachineFactory initialRawMaterials initialNonaMachineCost initialStorageLevel initialExpandStorageCost


initialBuildTime =
    5


initialRawMaterials =
    0


initialNonaMachineCost =
    100


initialStorageLevel =
    0


initialExpandStorageCost =
    500


initialNanomachineFactory : NanomachineFactory
initialNanomachineFactory =
    NanomachineFactory initialBuildTime



-- UPDATE


type Msg
    = Tick Time
    | ConvertNanomachine
    | ExpandStorage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | rawMaterials = updateRawMaterials model }, Cmd.none )

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

        ExpandStorage ->
            if model.rawMaterials >= model.expandStorageCost then
                ( { model
                    | rawMaterials = model.rawMaterials - model.expandStorageCost
                    , storageLevel = model.storageLevel + 1
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )


updateRawMaterials : Model -> Int
updateRawMaterials model =
    let
        newRawMaterials =
            model.rawMaterials + model.nanomachineQuantity

        totalStorage =
            currentStorage model
    in
        if newRawMaterials > totalStorage then
            totalStorage
        else
            newRawMaterials


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every second Tick ]



-- Helper function


currentStorage : Model -> Int
currentStorage model =
    1000 + model.storageLevel * 200



-- VIEW


nanomachineInfo : Int -> String
nanomachineInfo quantity =
    "Nanomachine: " ++ (toString quantity)


materialInfo : Int -> String
materialInfo quantity =
    "Raw Materials: " ++ (toString quantity)


storageInfo : Int -> String
storageInfo quantity =
    "Total Storage: " ++ (toString quantity)


viewNanomachine : Int -> Html Msg
viewNanomachine quantity =
    p [] [ text (nanomachineInfo quantity) ]


viewMaterials : Int -> Html Msg
viewMaterials quantity =
    p [] [ text (materialInfo quantity) ]


viewStorage : Int -> Html Msg
viewStorage quantity =
    p [] [ text (storageInfo quantity) ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewNanomachine model.nanomachineQuantity
        , viewMaterials model.rawMaterials
        , currentStorage model |> viewStorage
        , div [] [ button [ onClick ConvertNanomachine ] [ text ("Convert nanomachine (100 raw materials)") ] ]
        , div [] [ button [ onClick ExpandStorage ] [ text ("Expand Storage (500 raw materials)") ] ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

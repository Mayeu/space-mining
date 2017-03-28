module GrayGoo exposing (..)

import Data.Integer as Integer exposing (Integer)
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
    , localResource : Integer
    }


type alias NanomachineFactory =
    { buildTime : Int }


initialModel : Model
initialModel =
    Model 1 0 initialNanomachineFactory initialRawMaterials initialNonaMachineCost initialStorageLevel initialExpandStorageCost initialLocalResource


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


initialLocalResource =
    -- Local resources, in nano grams
    case Integer.fromString "32000000000000000000" of
        Just localResource ->
            localResource

        Nothing ->
            Integer.zero


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
            ( updateOnTick model, Cmd.none )

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


updateOnTick : Model -> Model
updateOnTick model =
    let
        newRawMaterials =
            model.rawMaterials + model.nanomachineQuantity

        totalStorage =
            currentStorage model
    in
        if newRawMaterials > totalStorage then
            { model
                | rawMaterials = totalStorage
                , localResource = Integer.sub model.localResource (Integer.fromInt (totalStorage - model.rawMaterials))
            }
        else
            { model
                | rawMaterials = newRawMaterials
                , localResource = Integer.sub model.localResource (Integer.fromInt model.nanomachineQuantity)
            }


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


resourceInfo : Integer -> String
resourceInfo quantity =
    "Local Resource: " ++ (Integer.toString quantity)


viewNanomachine : Int -> Html Msg
viewNanomachine quantity =
    p [] [ text (nanomachineInfo quantity) ]


viewMaterials : Int -> Html Msg
viewMaterials quantity =
    p [] [ text (materialInfo quantity) ]


viewStorage : Int -> Html Msg
viewStorage quantity =
    p [] [ text (storageInfo quantity) ]


viewResource : Integer -> Html Msg
viewResource quantity =
    p [] [ text (resourceInfo quantity) ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewNanomachine model.nanomachineQuantity
        , viewMaterials model.rawMaterials
        , currentStorage model |> viewStorage
        , viewResource model.localResource
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

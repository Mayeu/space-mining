module GrayGoo exposing (..)

import Data.Integer as I exposing (Integer)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time exposing (Time, second)


-- MODEL


type alias Model =
    { naniteQuantity : Integer
    , lastUpdate : Time
    , rawMaterials : Integer
    , naniteCost : Integer
    , storageLevel : Integer
    , expandStorageCost : Integer
    , localResource : Integer
    }


initialModel : Model
initialModel =
    Model I.one 0 initialRawMaterials initialNonaMachineCost initialStorageLevel initialExpandStorageCost initialLocalResource


initialBuildTime : Integer
initialBuildTime =
    I.fromInt 5


initialRawMaterials : Integer
initialRawMaterials =
    I.zero


initialNonaMachineCost : Integer
initialNonaMachineCost =
    I.fromInt 100


initialStorageLevel : Integer
initialStorageLevel =
    I.zero


initialExpandStorageCost : Integer
initialExpandStorageCost =
    I.fromInt 500


initialLocalResource : Integer
initialLocalResource =
    -- Local resources, in nano grams
    case I.fromString "32000000000000000000" of
        Just localResource ->
            localResource

        Nothing ->
            I.zero




-- UPDATE


type Msg
    = Tick Time
    | ConvertNanite
    | ExpandStorage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( updateOnTick model, Cmd.none )

        ConvertNanite ->
            if I.gte model.rawMaterials model.naniteCost then
                ( { model
                    | naniteQuantity = I.add model.naniteQuantity I.one
                    , rawMaterials = I.sub model.rawMaterials model.naniteCost
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        ExpandStorage ->
            if I.gte model.rawMaterials model.expandStorageCost then
                ( { model
                    | rawMaterials = I.sub model.rawMaterials model.expandStorageCost
                    , storageLevel = I.add model.storageLevel I.one
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )


updateOnTick : Model -> Model
updateOnTick model =
    let
        newRawMaterials =
            I.add model.rawMaterials model.naniteQuantity

        totalStorage =
            currentStorage model
    in
        if I.gt newRawMaterials totalStorage then
            { model
                | rawMaterials = totalStorage
                , localResource =
                  I.sub totalStorage model.rawMaterials
                |> I.sub model.localResource
            }
        else
            { model
                | rawMaterials = newRawMaterials
                , localResource = I.sub model.localResource model.naniteQuantity
            }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every second Tick ]



-- Helper function


currentStorage : Model -> Integer
currentStorage model =
    I.fromInt 200
        |> I.mul model.storageLevel
        |> I.add (I.fromInt 1000)



-- VIEW


naniteInfo : Integer -> String
naniteInfo quantity =
    "Nanite: " ++ (I.toString quantity)


materialInfo : Integer -> String
materialInfo quantity =
    "Raw Materials: " ++ (I.toString quantity)


storageInfo : Integer -> String
storageInfo quantity =
    "Total Storage: " ++ (I.toString quantity)


resourceInfo : Integer -> String
resourceInfo quantity =
    "Local Resource: " ++ (I.toString quantity)


viewNanite : Integer -> Html Msg
viewNanite quantity =
    p [] [ text (naniteInfo quantity) ]


viewMaterials : Integer -> Html Msg
viewMaterials quantity =
    p [] [ text (materialInfo quantity) ]


viewStorage : Integer -> Html Msg
viewStorage quantity =
    p [] [ text (storageInfo quantity) ]


viewResource : Integer -> Html Msg
viewResource quantity =
    p [] [ text (resourceInfo quantity) ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewNanite model.naniteQuantity
        , viewMaterials model.rawMaterials
        , currentStorage model |> viewStorage
        , viewResource model.localResource
        , div [] [ button [ onClick ConvertNanite ] [ text ("Convert nanite (100 raw materials)") ] ]
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

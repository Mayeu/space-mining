module GrayGoo exposing (..)

import Data.Integer as I exposing (Integer)
import Html exposing (..)
import Html.Attributes exposing (..)
import Material
import Material.Button as Button
import Material.Color as Color
import Material.Options as Options
import Material.Scheme
import Material.Toggles as Toggles
import Time exposing (Time, second)


-- MODEL


type alias NaniteModel =
    { quantity : Integer
    , storage : Integer
    , cost : Integer
    , autoreplication : Bool
    }


type alias Model =
    { nanite : NaniteModel
    , rawMaterials : Integer
    , storageLevel : Integer
    , expandStorageCost : Integer
    , localResource : Integer
    , mdl : Material.Model
    }


initialModel : Model
initialModel =
    Model initialNanite initialRawMaterials initialStorageLevel initialExpandStorageCost initialLocalResource Material.model


initialNanite : NaniteModel
initialNanite =
    NaniteModel I.one (I.fromInt 100) initialNaniteCost True


initialBuildTime : Integer
initialBuildTime =
    I.fromInt 5


initialRawMaterials : Integer
initialRawMaterials =
    I.zero


initialNaniteCost : Integer
initialNaniteCost =
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
    | ToggleAutoReplication
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( updateOnTick model, Cmd.none )

        ToggleAutoReplication ->
            let
                nanite =
                    model.nanite

                newNanite =
                    { nanite | autoreplication = not nanite.autoreplication }
            in
                ( { model | nanite = newNanite }, Cmd.none )

        ConvertNanite ->
            if I.gte model.rawMaterials model.nanite.cost then
                let
                    nanite =
                        model.nanite

                    newNanite =
                        { nanite | quantity = I.add nanite.quantity I.one }
                in
                    ( { model
                        | nanite = newNanite
                        , rawMaterials = I.sub model.rawMaterials model.nanite.cost
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

        Mdl msg_ ->
            Material.update Mdl msg_ model


updateOnTick : Model -> Model
updateOnTick model =
    let
        newRawMaterials =
            I.add model.rawMaterials model.nanite.quantity

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
                , localResource = I.sub model.localResource model.nanite.quantity
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


type alias Mdl =
    Material.Model


naniteInfo : Integer -> String
naniteInfo quantity =
    "Nanite: " ++ (I.toString quantity)


materialInfo : Integer -> Integer -> String
materialInfo quantity storage =
    "Raw Materials: " ++ (I.toString quantity) ++ " / " ++ (I.toString storage)


storageInfo : Integer -> String
storageInfo quantity =
    "Total Storage: " ++ (I.toString quantity)


resourceInfo : Integer -> String
resourceInfo quantity =
    "Local Resource: " ++ (I.toString quantity)


viewNanite : Integer -> Html Msg
viewNanite quantity =
    p [] [ text (naniteInfo quantity) ]


viewMaterials : Integer -> Integer -> Html Msg
viewMaterials quantity storage =
    p [] [ text (materialInfo quantity storage) ]


viewResource : Integer -> Html Msg
viewResource quantity =
    p [] [ text (resourceInfo quantity) ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewNanite model.nanite.quantity
        , currentStorage model |> viewMaterials model.rawMaterials
        , viewResource model.localResource
        , Toggles.switch Mdl
            [ 0 ]
            model.mdl
            [ Options.onToggle ToggleAutoReplication
            , Toggles.value model.nanite.autoreplication
            ]
            [ text "Autoreplication" ]
        , Button.render Mdl
            [ 1 ]
            model.mdl
            [ Button.raised
            , if model.nanite.autoreplication then
                Options.nop
              else
                Button.disabled
            , Options.onClick ConvertNanite
            ]
            [ text ("Convert nanite (100 raw materials)") ]
        , Button.render Mdl
            [ 2 ]
            model.mdl
            [ Button.raised
            , Options.onClick ExpandStorage
            ]
            [ text ("Expand Storage (500 raw materials)") ]
        ]
        |> Material.Scheme.topWithScheme Color.Grey Color.Blue


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

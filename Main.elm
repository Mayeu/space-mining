module GrayGoo exposing (..)

import Data.Integer as I exposing (Integer)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
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
    }


initialModel : Model
initialModel =
    Model initialNanite initialRawMaterials initialStorageLevel initialExpandStorageCost initialLocalResource


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
    | ToggleAutoreplication


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( updateOnTick model, Cmd.none )

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

        ToggleAutoreplication ->
            let
                nanite =
                    model.nanite

                newNanite =
                    { nanite | autoreplication = not nanite.autoreplication }
            in
                ( { model | nanite = newNanite }, Cmd.none )


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


resourceInfo : Integer -> String
resourceInfo quantity =
    "Local Resource: " ++ (I.toString quantity)


viewResource : String -> Integer -> Html Msg
viewResource label quantity =
    p [] [ (label ++ ": " ++ (I.toString quantity)) |> text ]


viewMaterials : Integer -> Integer -> Html Msg
viewMaterials quantity storage =
    p [] [ text (materialInfo quantity storage) ]


materialInfo : Integer -> Integer -> String
materialInfo quantity storage =
    "Raw Materials: " ++ (I.toString quantity) ++ " / " ++ (I.toString storage)


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewResource "Nanite" model.nanite.quantity
        , currentStorage model |> viewMaterials model.rawMaterials
        , viewResource "Local Resource" model.localResource
        , div []
            [ checkbox ToggleAutoreplication
                "Autoreplication"
                model.nanite.autoreplication
            , button
                [ onClick ConvertNanite
                , if model.nanite.autoreplication then
                    disabled True
                  else
                    disabled False
                ]
                [ text ("Convert nanite (100 raw materials)") ]
            , button
                [ onClick ExpandStorage ]
                [ text ("Expand Storage (500 raw materials)") ]
            ]
        ]


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name state =
    label
        [ style [ ( "padding", "20px" ) ]
        ]
        [ input [ type_ "checkbox", checked state, onClick msg ] []
        , text name
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

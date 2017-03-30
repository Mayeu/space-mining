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


type Resource
    = RawMaterial
    | Nanite


type alias Trigger =
    { resource : Resource
    , quantity : Integer
    }


type InterfaceItem
    = Button
    | Checkbox


type alias InterfaceModel =
    { type_ : InterfaceItem
    , text : String
    , msg : Msg
    , trigger : Trigger
    , triggered : Bool
    }


type alias Model =
    { nanite : NaniteModel
    , rawMaterials : Integer
    , localResource : Integer
    , interface : InterfaceModel
    }


interfaceModel : InterfaceModel
interfaceModel =
    { type_ = Button
    , text = "Convert nanite"
    , msg = ConvertNanite
    , trigger = Trigger RawMaterial (I.fromInt 100)
    , triggered = False
    }


initialModel : Model
initialModel =
    Model initialNanite initialRawMaterials initialLocalResource interfaceModel


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
    {-
       On each tick there is multiple things to resolve.
       Each of those should take the model as input, and output the model
       (modified or not).
    -}
    model
        |> updateMining
        |> updateTrigger
        |> updateReplicate



{-
   Update what can be trigger on the interface
-}


updateTrigger : Model -> Model
updateTrigger model =
    -- Right now, there is only one button
    if I.gte model.rawMaterials model.interface.trigger.quantity then
        let
            interface =
                model.interface

            newInterface =
                { interface | triggered = True }
        in
            { model | interface = newInterface }
    else
        model


updateMining : Model -> Model
updateMining model =
    let
        mined =
            mine model
    in
        if I.eq mined I.zero then
            model
        else
            { model
                | rawMaterials =
                    I.add model.rawMaterials mined
                , localResource =
                    I.sub model.localResource mined
            }


updateReplicate : Model -> Model
updateReplicate model =
    case model.nanite.autoreplication of
        False ->
            model

        True ->
            if I.eq model.rawMaterials <| currentStorage model then
                let
                    double value =
                        I.mul value (I.fromInt 2)

                    nanite =
                        model.nanite

                    newNanite =
                        { nanite | quantity = double nanite.quantity }
                in
                    { model
                        | rawMaterials = I.zero
                        , nanite = newNanite
                        , localResource = I.sub model.localResource model.rawMaterials
                    }
            else
                model



{- Given a model, return the mined quantity -}


mine : Model -> Integer
mine model =
    let
        totalStorage =
            currentStorage model

        leftStorage =
            I.sub totalStorage model.rawMaterials
    in
        if I.eq model.rawMaterials totalStorage then
            I.zero
        else
            I.min model.nanite.quantity <| I.min model.localResource leftStorage



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every second Tick ]



-- Helper function


currentStorage : Model -> Integer
currentStorage model =
    I.mul model.nanite.storage model.nanite.quantity



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
        , checkbox ToggleAutoreplication
            "Autoreplication"
            model.nanite.autoreplication
        , currentStorage model |> viewMaterials model.rawMaterials
        , viewResource "Local Resource" model.localResource
        , generateInterface model
        ]


generateInterface : Model -> Html Msg
generateInterface model =
    let
        interface =
            model.interface
    in
        case interface.type_ of
            Button ->
                if interface.triggered then
                    div []
                        [ button
                            [ onClick interface.msg
                            , if model.nanite.autoreplication then
                                disabled True
                              else
                                disabled False
                            ]
                            [ text (interface.text ++ " (" ++ (toString 100) ++ " raw materials)") ]
                        ]
                else
                    div [] []

            Checkbox ->
                div [] [ (checkbox interface.msg interface.text True) ]


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name state =
    label
        [ style [ ( "padding", "10px" ) ]
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

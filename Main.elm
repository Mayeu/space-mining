module SpaceMining exposing (..)

import Data.Integer as I exposing (Integer)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time exposing (Time, second)


-- MODEL


type PlayerStatus
    = Idling
    | Mining
    | Dead


type Resource
    = RawMaterial
    | Oxygen


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
    { rawMaterials : Integer
    , oxygen : Integer
    , spaceCoins : Integer
    , interface : List InterfaceModel
    , playerStatus : PlayerStatus
    }


interfaceModel : List InterfaceModel
interfaceModel =
    [ { type_ = Button
      , text = "Mine"
      , msg = Mine
      , trigger = Trigger RawMaterial I.zero
      , triggered = True
      }
    , { type_ = Button
      , text = "Buy 25 O2 (25 SpaceCoins)"
      , msg = BuyOxygen
      , trigger = Trigger Oxygen I.zero
      , triggered = True
      }
    , { type_ = Button
      , text = "Sell 10 raw materials for 25 SC"
      , msg = Sell10RawMaterials
      , trigger = Trigger RawMaterial (I.fromInt 10)
      , triggered = False
      }
    ]


initialModel : Model
initialModel =
    Model initialRawMaterials initialOxygen initialSpaceCoin interfaceModel Idling


initialBuildTime : Integer
initialBuildTime =
    I.fromInt 5


initialRawMaterials : Integer
initialRawMaterials =
    I.zero


initialOxygen : Integer
initialOxygen =
    I.fromInt 100


initialSpaceCoin : Integer
initialSpaceCoin =
    I.fromInt 100


oxygenCost : Integer
oxygenCost =
    I.one



-- UPDATE


type Msg
    = Tick Time
    | BuyOxygen
    | Mine
    | Sell10RawMaterials


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( updateOnTick model, Cmd.none )

        BuyOxygen ->
            ( buyOxygen model (I.fromInt 25), Cmd.none )

        Mine ->
            ( { model
                | rawMaterials = I.add model.rawMaterials I.one
              }
                |> updateOxygen
                |> updateTrigger
            , Cmd.none
            )

        Sell10RawMaterials ->
            ( sellRawMaterials model (I.fromInt 10) (I.fromInt 25)
            , Cmd.none
            )


sellRawMaterials : Model -> Integer -> Integer -> Model
sellRawMaterials model quantity price =
    if I.lte quantity model.rawMaterials then
        { model
            | rawMaterials = I.sub model.rawMaterials quantity
            , spaceCoins = I.add model.spaceCoins price
        }
    else
        model


buyOxygen : Model -> Integer -> Model
buyOxygen model quantity =
    let
        buyingCost =
            I.mul quantity oxygenCost
    in
        if I.gte model.spaceCoins buyingCost then
            { model
                | oxygen = I.add model.oxygen quantity
                , spaceCoins = I.sub model.spaceCoins buyingCost
            }
        else
            model


updateOnTick : Model -> Model
updateOnTick model =
    {-
       On each tick there is multiple things to resolve.
       Each of those should take the model as input, and output the model
       (modified or not).
    -}
    model
        |> updateOxygen
        |> updateTrigger



{-
   The oxygen automatically go down over time
-}


updateOxygen : Model -> Model
updateOxygen model =
    let
        newOxygen =
            I.sub model.oxygen I.one
    in
        if I.gt newOxygen I.zero then
            { model | oxygen = newOxygen }
        else
            { model
                | oxygen = I.zero
                , playerStatus = Dead
            }



{-
   Update what can be trigger on the interface
-}


updateTrigger : Model -> Model
updateTrigger model =
    let
        interfaces =
            model.interface

        newInterfaces =
            List.map (setTrigger model) interfaces
    in
        { model | interface = newInterfaces }


setTrigger : Model -> InterfaceModel -> InterfaceModel
setTrigger model interface =
    -- Right now, there is only one button
    case interface.trigger.resource of
        RawMaterial ->
            if I.gte model.rawMaterials interface.trigger.quantity then
                { interface | triggered = True }
            else
                interface

        Oxygen ->
            if I.gte model.oxygen interface.trigger.quantity then
                { interface | triggered = True }
            else
                interface



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every second Tick ]



-- Helper function


currentStorage : Model -> Integer
currentStorage model =
    I.fromInt 100



-- VIEW


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
        [ viewResource "O2 Level" model.oxygen
        , viewResource "SpaceCoin" model.spaceCoins
        , currentStorage model |> viewMaterials model.rawMaterials
        , mapInterface model
        , if model.playerStatus == Dead then
            p [] [ text "Sorry, you are dead" ]
          else
            p [] []
        ]


mapInterface : Model -> Html Msg
mapInterface model =
    div [] <| List.map (generateInterface model) model.interface


generateInterface : Model -> InterfaceModel -> Html Msg
generateInterface model interface =
    case interface.type_ of
        Button ->
            if interface.triggered then
                div []
                    [ button
                        [ onClick interface.msg
                        , if model.playerStatus == Dead then
                            disabled True
                          else
                            disabled False
                        ]
                        [ text (interface.text) ]
                    ]
            else
                div [] []

        Checkbox ->
            div [] [ (checkbox interface.msg interface.text False) ]


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

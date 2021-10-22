module Main exposing (..)

import Browser
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Map exposing (Location, Route)


type Message
    = SetName String
    | SetLatitude String
    | SetLongitude String
    | AddLocation
    | RemoveLocation Int
    | Calculate


type alias Model =
    { name : Maybe String
    , latitude : Maybe String
    , longitude : Maybe String
    , route : Route
    , status : String
    , idCounter : Int
    }


init : Model
init =
    { name = Nothing
    , latitude = Nothing
    , longitude = Nothing
    , route = []
    , status = "Please input some coordinates."
    , idCounter = 0
    }

addLocation : Location -> Route -> Route
addLocation loc route =
    case route of
        x :: xs -> x :: addLocation loc xs
        [] -> [loc]

removeLocation : Int -> Route -> Route
removeLocation id route =
    case route of
        x :: xs ->
            if x.id == id
            then xs
            else x :: removeLocation id xs
        [] -> []

update : Message -> Model -> Model
update message model =
    let maybeLocation =
            Maybe.map3 (Map.location model.idCounter)
                model.name
                (Maybe.andThen String.toFloat model.latitude)
                (Maybe.andThen String.toFloat model.longitude)
    in case message of
          AddLocation ->
            case maybeLocation of
                Just loc ->
                    { model | route = addLocation loc model.route
                    , idCounter = model.idCounter + 1
                    }
                Nothing ->
                    { model | status = "Error, Can't add an invalid location!" }
          RemoveLocation id ->
            { model | route =  removeLocation id model.route }
          SetName name ->
            { model | name = Just name }
          SetLatitude lat ->
            { model | latitude = Just lat }
          SetLongitude long ->
            { model | longitude = Just long }
          Calculate ->
            { model | route = Map.twoOpt model.route }

blue : Color
blue = rgb255 0x02 0x69 0xA4

white : Color
white = rgb255 0xFF 0xFF 0xFF

styledButton : List (Attr () msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
styledButton attrs body =
    let styles = [ Font.color white, Background.color blue, padding 10 ]
    in Input.button (styles ++ attrs) body

showRoute : Route -> Element Message
showRoute route =
    column [ spacing 10, paddingXY 10 5 ] <| List.map (text << .name) route

view : Model -> Html Message
view model =
    layout [ paddingXY 10 10 ] <|
        column [ spacing 10 ]
            [ text model.status
            , row [ ]
                [ table [ spacing 20, paddingXY 10 5 ]
                    { data = model.route
                    , columns =
                        [ { header = text "Name"
                          , width = fill
                          , view = \loc -> el [ centerX, centerY ] <| text loc.name
                          }
                        , { header = text "Latitude"
                          , width = fill
                          , view = \loc -> el [ centerX, centerY ] <| text <| String.fromFloat loc.latitude
                          }
                        , { header = text "Longitude"
                          , width = fill
                          , view = \loc -> el [ centerX, centerY ] <| text <| String.fromFloat loc.longitude
                          }
                        , { header = text "Actions"
                          , width = fill
                          , view = \loc ->
                                styledButton [ height fill ]
                                    { onPress = Just <| RemoveLocation loc.id
                                    , label = text "Remove"
                                    }
                          }
                        ]
                    }
                ]
            , column [ spacing 10, paddingXY 0 10 ]
                [ Input.text []
                    { onChange = SetName
                    , placeholder = Just <| Input.placeholder [] <| text "Name"
                    , text = case model.name of
                                Just name -> name
                                Nothing -> ""
                    , label = Input.labelHidden "Name"
                    }
                , Input.text []
                    { onChange = SetLatitude
                    , placeholder = Just <| Input.placeholder [] <| text "Latitude"
                    , text = case model.latitude of
                                Just lat -> lat
                                Nothing -> ""
                    , label = Input.labelHidden "Latitude"
                    }
                , Input.text []
                    { onChange = SetLongitude
                    , placeholder = Just <| Input.placeholder [] <| text "Longitude"
                    , text = case model.longitude of
                                Just long -> long
                                Nothing -> ""
                    , label = Input.labelHidden "Longitude"
                    }
                , row [ spacing 10 ]
                    [ styledButton []
                        { onPress = Just AddLocation
                        , label = el [ Font.color white ] <| text "Add Location"
                        }
                    , styledButton []
                        { onPress = Just Calculate
                        , label = el [ Font.color white ] <| text "Calculate"
                        }
                    ]
                ]
            ]

main : Program () Model Message
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

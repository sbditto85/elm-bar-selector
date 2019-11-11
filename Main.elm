module Main exposing(..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Time exposing (Time)
import Tuple exposing (first)

onMouseMove : msg -> Attribute msg
onMouseMove msg =
    onWithOptions "mousemove" { stopPropagation = True
                              , preventDefault = True
                              } (Json.succeed msg)

type Msg
    = NoOp
    | Clicked Time
    | MouseDown Time
    | MouseUp Time
    | MouseMove Time
    | MouseLeave


type alias Model =
    { times : List Time
    , clicked : Maybe Time
    , current_selection : (Maybe Time, Maybe Time)
    }

model : Model
model =
    { times = [1,2,3,4,5,6,7,8,9,10]
    , clicked = Nothing
    , current_selection = (Nothing, Nothing)
    }


view_selector : List Time -> (Maybe Time, Maybe Time) -> Html Msg
view_selector times (mStart, mEnd) =
    let
        extra_style t =
            case mStart of
                Nothing ->
                    []
                Just s ->
                    case mEnd of
                        Nothing ->
                            if s == t then
                                [ ("background-color", "yellow") ]
                            else
                                []
                        Just e ->
                            if s <= t && t <= e then
                                [ ("background-color", "yellow") ]
                            else
                                []
        times_divs =
            List.map (\t ->
                          div
                           [ style ([ ("position","relaitve")
                                    , ("display","inline-block")
                                    , ("width","48px")
                                    , ("height","100px")
                                    , ("border","1px solid #000")
                                    ] ++ extra_style t
                                   )
                           , onClick (Clicked t)
                           , onMouseDown (MouseDown t)
                           , onMouseUp (MouseUp t)
                           , onMouseMove (MouseMove t)
                           ]
                           [ text (toString t)
                           ]
                     ) times
    in
    div
      [ style [ ("position","relaitve")
              , ("margin","50px")
              , ("width","500px")
              , ("height","100px")
              , ("border","1px solid #000")
              ]
      , onMouseLeave MouseLeave
      ]
      times_divs

view_clicked : Maybe Time -> Html Msg
view_clicked mClicked =
    case mClicked of
        Nothing ->
            text ""
        Just t ->
            text (toString t)

view_current_selection : (Maybe Time, Maybe Time) -> Html Msg
view_current_selection (mStart, mEnd) =
    let
        start =
            case mStart of
                Nothing ->
                    text ""
                Just s ->
                    p [] [ text (toString s) ]
        end =
            case mEnd of
                Nothing ->
                    text ""
                Just e ->
                    p [] [ text (toString e) ]
    in
        div [] [ start
               , end
               ]

view : Model -> Html Msg
view model =
    div []
        [ view_selector model.times model.current_selection
        , view_clicked model.clicked
        , view_current_selection model.current_selection
        ]

update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model
        Clicked t ->
            { model | clicked = Just t }
        MouseDown t ->
            { model | current_selection = (Just t, Nothing) }
        MouseUp t ->
            { model | current_selection = (first model.current_selection, Just t) }
        MouseMove t ->
            { model | current_selection = (first model.current_selection, Just t) }
        MouseLeave ->
            { model | current_selection = (Nothing, Nothing) }


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }

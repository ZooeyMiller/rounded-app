port module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program (Maybe Model) Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- Model


type alias Model =
    { name : String
    , confirmedName : String
    , location : String
    , current : EmotionDatum
    , emotionHistory : Array EmotionDatum
    }


type alias EmotionDatum =
    { mood : String
    , energy : String
    }


init : Maybe Model -> Navigation.Location -> ( Model, Cmd Msg )
init model location =
    case model of
        Just model ->
            ( { model | location = location.pathname }, Cmd.none )

        Nothing ->
            ( Model "" "" location.pathname (EmotionDatum "5" "5") (Array.fromList []), Cmd.none )



--Update


type Msg
    = Name String
    | LoginSubmit
    | EmotionToGraph
    | EmotionToEmotion
    | UrlChange Navigation.Location
    | Mood String
    | Energy String
    | NoMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Name newName ->
            ( { model | name = newName }, Cmd.none )

        LoginSubmit ->
          let newModel =
            { model | confirmedName = model.name }
          in
            ( newModel, Cmd.batch [ Navigation.newUrl "/mood", setStorage newModel ] )

        EmotionToGraph ->
          let newModel =
            { model | emotionHistory = push model.current model.emotionHistory }
          in
            ( newModel, Cmd.batch [ setStorage newModel, Navigation.newUrl "/graph" ] )

        EmotionToEmotion ->
            ( { model | emotionHistory = push model.current model.emotionHistory }, Cmd.none )

        Mood newMood ->
            ( { model | current = setMood newMood model.current }, Cmd.none )

        Energy newMood ->
            ( { model | current = setEnergy newMood model.current }, Cmd.none )

        UrlChange newLocation ->
            ( { model | location = newLocation.pathname }, Cmd.none )

        NoMsg ->
          ( model, Cmd.none)


setEnergy : String -> EmotionDatum -> EmotionDatum
setEnergy newEnergy mood =
    { mood | energy = newEnergy }


setMood : String -> EmotionDatum -> EmotionDatum
setMood newMood mood =
    { mood | mood = newMood }



--View


view : Model -> Html Msg
view model =
    div [ Html.Attributes.class "flex flex-column vh-100" ]
        [ header [ Html.Attributes.class "pa3 bg-blue" ]
            [ span [ Html.Attributes.class "f3 ma0 tracked white" ] [ Html.text "☉ rounded" ]
            ]
        , div [Html.Attributes.class "pa4 flex-auto h-100"] [main_ [ Html.Attributes.class "flex flex-column justify-center w-100 h-100 pa4 mw6 center ba br2 br4--top-right br4--bottom-left br--bottom-right b--blue bg-light-gray pa4 br2-m" ]
            [ chooseView model ]
            ]
        ]


isNamed model viewFunction =
  case model.confirmedName of
    "" ->
      loginView model
    _ ->
      viewFunction model

chooseView : Model -> Html Msg
chooseView model =
    case model.location of
        "/" ->
            loginView model

        "/mood" ->
            isNamed model emotionView

        "/graph" ->
            isNamed model graphView

        _ ->
            div [] [ Html.text "you're not supposed to be here" ]


loginView : Model -> Html Msg
loginView model =
  div [] [
    h1 [] [ Html.text "Welcome to rounded" ],
    p [] [ Html.text "A way for you to keep track of variations in your mood."],
    p [] [ Html.text "Enter your name to continue:"],
    Html.form [ onSubmit LoginSubmit, Html.Attributes.class "flex flex-column items-center justify-center flex-auto pa4" ]
        [ label [ for "name", Html.Attributes.class "vh" ]
            [ Html.text "Name" ]
        , input [ Html.Attributes.id "name", placeholder "Name", Html.Attributes.type_ "text", value model.name, onInput Name, Html.Attributes.class "db w-100 center pa2 bn" ]
            []
        , button [ Html.Attributes.type_ "submit", Html.Attributes.class "grow bn ph3 pv2 white bg-blue db w-100 center mt3" ]
            [ Html.text "Go" ]
        ]
      ]


emotionView : Model -> Html Msg
emotionView model =
    div []
        [
          p [] [ Html.text <| emotionViewPrompt model ],
          Html.form [ onSubmit (emotionRouting model) ]
            [ label [ for "mood" ]
                [ Html.text "Mood" ]
            , input [ Html.Attributes.id "mood", onInput Mood, Html.Attributes.max "10", Html.Attributes.min "0", value model.current.mood, Html.Attributes.type_ "range" ]
                []
            , label [ for "energy" ]
                [ Html.text "Energy" ]
            , input [ Html.Attributes.id "energy", onInput Energy, Html.Attributes.max "10", Html.Attributes.min "0", value model.current.energy, Html.Attributes.type_ "range" ]
                []
            , button [ Html.Attributes.type_ "submit" ]
                [ Html.text "Submit" ]
            ]
        ]

emotionRouting : Model -> Msg
emotionRouting model =
  case (Array.length model.emotionHistory) of
    0 ->
      EmotionToEmotion
    1 ->
      EmotionToEmotion
    _ ->
      EmotionToGraph

emotionViewPrompt : Model -> String
emotionViewPrompt model =
  case (Array.length model.emotionHistory) of
    0 ->
      "How were you feeling 2 days ago?"
    1 ->
      "How were you feeling yesterday?"
    _ ->
      "How are you feeling now?"


graphView : Model -> Html Msg
graphView model =
    Html.div [ Html.Attributes.class "relative h5 bl bb bw2 b--mid-gray overflow-x-scroll" ]
        [ svg [ Svg.Attributes.class "absolute h-100", viewBox ("0 0 " ++ (toString <| Array.length model.emotionHistory) ++ " 11"), Svg.Attributes.strokeWidth "0.15" ]
            (plotGraph "mood" model.emotionHistory
                ++ plotGraph "energy" model.emotionHistory
            )
        ]


plotGraph : String -> Array EmotionDatum -> List (Svg Msg)
plotGraph dataType array =
    let
        emotionType =
            if dataType == "mood" then
                .mood
            else
                .energy
    in
    Array.toList (Array.indexedMap (\i emotion -> graphPoint i (emotionType emotion) array dataType) array)


graphPoint : Int -> String -> Array EmotionDatum -> String -> Svg Msg
graphPoint index y array toPlot =
    let
        dataType =
            if toPlot == "mood" then
                .mood
            else
                .energy

        dataColour =
            if toPlot == "mood" then
                "orange"
            else
                "purple"
    in
    if index == 0 then
        circle
            [ cx (toString index)
            , cy (stringNumMinusNum y 11)
            , r "0.2"
            , Svg.Attributes.style ("fill: " ++ dataColour ++ "; stroke: " ++ dataColour ++ ";")
            ]
            []
    else
        g []
            [ circle
                [ cx (toString index)
                , cy (stringNumMinusNum y 11)
                , r "0.2"
                , Svg.Attributes.style ("fill: " ++ dataColour ++ "; stroke: " ++ dataColour ++ ";")
                ]
                []
            , Svg.path
                [ d
                    ("M"
                        ++ toString (index - 1)
                        ++ " "
                        ++ stringNumMinusNum (dataType <| getPoint <| Array.get (index - 1) array) 11
                        ++ " L"
                        ++ (toString <| index)
                        ++ " "
                        ++ stringNumMinusNum y 11
                    )
                , Svg.Attributes.style ("stroke: " ++ dataColour)
                ]
                []
            ]


getPoint : Maybe EmotionDatum -> EmotionDatum
getPoint point =
    case point of
        Nothing ->
            { mood = "0", energy = "0" }

        Just val ->
            val


stringNumMinusNum : String -> Int -> String
stringNumMinusNum stringNum num =
    toString <| num - (Result.withDefault 0 <| String.toInt stringNum)


port setStorage : Model -> Cmd msg

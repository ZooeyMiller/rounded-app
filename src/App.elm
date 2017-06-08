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
    { username : String
    , password : String
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
    = Username String
    | Password String
    | LoginSubmit
    | EmotionSubmit
    | UrlChange Navigation.Location
    | Mood String
    | Energy String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Username newUsername ->
            ( { model | username = newUsername }, Cmd.none )

        Password newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        LoginSubmit ->
            ( model, Cmd.batch [ Navigation.newUrl "/mood", setStorage model ] )

        EmotionSubmit ->
            ( { model | emotionHistory = push model.current model.emotionHistory }, Cmd.batch [ Navigation.newUrl "/graph", setStorage model ] )

        Mood newMood ->
            ( { model | current = setMood newMood model.current }, Cmd.none )

        Energy newMood ->
            ( { model | current = setEnergy newMood model.current }, Cmd.none )

        UrlChange newLocation ->
            ( { model | location = newLocation.pathname }, Cmd.none )


setEnergy : String -> EmotionDatum -> EmotionDatum
setEnergy newEnergy mood =
    { mood | energy = newEnergy }


setMood : String -> EmotionDatum -> EmotionDatum
setMood newMood mood =
    { mood | mood = newMood }



--View


view : Model -> Html Msg
view model =
    div []
        [ chooseView model
        ]


chooseView : Model -> Html Msg
chooseView model =
    case model.location of
        "/" ->
            loginForm model

        "/mood" ->
            recordMood model

        "/graph" ->
            graphView model

        _ ->
            div [] [ Html.text "you're not supposed to be here" ]


loginForm : Model -> Html Msg
loginForm model =
    div []
        [ Html.form [ onSubmit LoginSubmit, Html.Attributes.class "flex flex-column items-center justify-center flex-auto ba br2 br4--top-right br4--bottom-left br--bottom-right b--blue bg-black-05 pa4 br2-m" ]
            [ label [ for "username", Html.Attributes.class "vh" ]
                [ Html.text "Username" ]
            , input [ Html.Attributes.id "username", placeholder "Username", Html.Attributes.type_ "text", value model.username, onInput Username, Html.Attributes.class "db w-100 center pa2 bn" ]
                []
            , label [ for "password", Html.Attributes.class "vh" ]
                [ Html.text "Password" ]
            , input [ Html.Attributes.id "password", placeholder "password", Html.Attributes.type_ "password", value model.password, onInput Password, Html.Attributes.class "db w-100 center pa2 bn mt3" ]
                []
            , button [ Html.Attributes.type_ "submit", Html.Attributes.class "grow bn ph3 pv2 white bg-blue db w-100 center mt3" ]
                [ Html.text "Log in" ]
            ]
        ]


recordMood : Model -> Html Msg
recordMood model =
    Html.form [ onSubmit EmotionSubmit ]
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


graphView : Model -> Html Msg
graphView model =
    Html.div [ Html.Attributes.class "h5 bl bb bw2 overflow-x-scroll" ]
        [ svg [ Svg.Attributes.class "h-100", viewBox ("0 0 " ++ (toString <| Array.length model.emotionHistory) ++ " 11"), Svg.Attributes.strokeWidth "0.1" ]
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

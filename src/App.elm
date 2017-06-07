module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- Model


type alias Model =
    { username : String
    , password : String
    , location : Navigation.Location
    , current : EmotionDatum
    , emotionHistory : Array EmotionDatum
    }


type alias EmotionDatum =
    { mood : String
    , energy : String
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( Model "" "" location (EmotionDatum "5" "5") (Array.fromList [ EmotionDatum "1" "2", EmotionDatum "8" "2", EmotionDatum "10" "10", EmotionDatum "7" "5", EmotionDatum "8" "2" ]), Cmd.none )



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
            ( model, Navigation.newUrl "#/mood" )

        EmotionSubmit ->
            ( model, Navigation.newUrl "#/graph" )

        Mood newMood ->
            ( { model | current = setMood newMood model.current }, Cmd.none )

        Energy newMood ->
            ( { model | current = setEnergy newMood model.current }, Cmd.none )

        UrlChange newLocation ->
            ( { model | location = newLocation }, Cmd.none )


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


chooseView model =
    case model.location.hash of
        "" ->
            loginForm model

        "#/mood" ->
            recordMood model

        "#/graph" ->
            graphView model

        _ ->
            div [] [ Html.text "you're not supposed to be here" ]


loginForm : Model -> Html Msg
loginForm model =
    Html.form [ onSubmit LoginSubmit ]
        [ label [ for "username" ]
            [ Html.text "Username" ]
        , input [ Html.Attributes.id "username", placeholder "Username", Html.Attributes.type_ "text", value model.username, onInput Username ]
            []
        , label [ for "password" ]
            [ Html.text "Password" ]
        , input [ placeholder "password", Html.Attributes.type_ "password", value model.password, onInput Password ]
            []
        , button [ Html.Attributes.type_ "submit" ]
            [ Html.text "Log in" ]
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
    Html.div []
        [ svg [ Svg.Attributes.height "200", viewBox ("0 0 " ++ (toString <| Array.length model.emotionHistory) ++ " 11"), Svg.Attributes.style "border-bottom: 3px solid black; stroke: black; stroke-width: 0.1; border-left: 3px solid black; margin-left: 5rem; margin-top: 5rem; " ]
            ( (Array.toList (Array.indexedMap (\i emotion -> graphPoint i emotion.mood model.emotionHistory "mood") model.emotionHistory)) ++
             (Array.toList (Array.indexedMap (\i emotion -> graphPoint i emotion.energy model.emotionHistory "energy") model.emotionHistory)) )
        ]


-- graphPoint : Int -> String -> Array EmotionDatum -> String -> Svg Msg
graphPoint index y array toPlot =
    let dataType =
      if toPlot == "mood" then
        .mood
      else .energy
    in
      if index == 0 then
        circle [ cx (toString index), cy (stringNumMinusNum y 11), r "0.2" ] []
      else
        g []
            [ circle [ cx (toString index), cy (stringNumMinusNum y 11), r "0.2" ] []
            , Svg.path [ d ("M" ++ toString (index - 1) ++ " " ++
                stringNumMinusNum (dataType <| getPoint <| Array.get (index - 1) array) 11 ++
                " L" ++
                (toString <| index) ++
                " " ++
                stringNumMinusNum y 11)
              ] []
            ]

getPoint point =
    case point of
        Nothing ->
            { mood = "0", energy = "0" }

        Just val ->
            val


stringNumMinusNum : String -> Int -> String
stringNumMinusNum stringNum num =
    toString <| num - (Result.withDefault 0 <| String.toInt stringNum)

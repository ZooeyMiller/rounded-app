module Main exposing (..)

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
    , current : CurrentMood
    }


type alias CurrentMood =
    { mood : String
    , energy : String
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( Model "" "" location (CurrentMood "5" "5"), Cmd.none )



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

        UrlChange location ->
            ( { model | location = location }, Cmd.none )


setEnergy : String -> CurrentMood -> CurrentMood
setEnergy newEnergy mood =
    { mood | energy = newEnergy }


setMood : String -> CurrentMood -> CurrentMood
setMood newMood mood =
    { mood | mood = newMood }



--View


view : Model -> Html Msg
view model =
    div []
        [ chooseView model
        , div [] [ Html.text model.location.pathname ]
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
        [ svg [ Svg.Attributes.width "200", Svg.Attributes.height "200", viewBox "0 0 100 100" ]
            [ circle [ cx "50", cy "50", r "50" ] [] ]
        ]

module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation


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
    | Submit
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

        Submit ->
            ( model, Navigation.newUrl "#/mood" )

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
        , div [] [ text model.location.pathname ]
        ]


loginForm : Model -> Html Msg
loginForm model =
    Html.form [ onSubmit Submit ]
        [ label [ for "username" ]
            [ text "Username" ]
        , input [ id "username", placeholder "Username", type_ "text", value model.username, onInput Username ]
            []
        , label [ for "password" ]
            [ text "Password" ]
        , input [ placeholder "password", type_ "password", value model.password, onInput Password ]
            []
        , button [ type_ "submit" ]
            [ text "Log in" ]
        ]


recordMood : Model -> Html Msg
recordMood model =
    Html.form []
        [ label [ for "mood" ]
            [ text "Mood" ]
        , input [ id "mood", onInput Mood, Html.Attributes.max "10", Html.Attributes.min "0", value model.current.mood, type_ "range" ]
            []
        , label [ for "energy" ]
            [ text "Energy" ]
        , input [ id "energy", onInput Energy, Html.Attributes.max "10", Html.Attributes.min "0", value model.current.energy, type_ "range" ]
            []
        , button [ type_ "submit" ]
            [ text "Submit" ]
        ]


chooseView model =
    case model.location.hash of
        "" ->
            loginForm model

        "#/mood" ->
            recordMood model

        _ ->
            div [] [ text "you're not supposed to be here" ]

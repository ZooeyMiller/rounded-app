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
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( Model "" "" location, Cmd.none )



--Update


type Msg
    = Username String
    | Password String
    | Submit
    | UrlChange Navigation.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Username newUsername ->
            ( { model | username = newUsername }, Cmd.none )

        Password newPassword ->
            ( { model | password = newPassword }, Cmd.none )

        Submit ->
            ( { model | location = model.location }, Navigation.newUrl "#mood" )

        UrlChange location ->
            ( model, Cmd.none )



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
        , input [ id "mood", Html.Attributes.max "10", Html.Attributes.min "0", type_ "range" ]
            []
        , label [ for "energy" ]
            [ text "Energy" ]
        , input [ id "energy", Html.Attributes.max "10", Html.Attributes.min "0", type_ "range" ]
            []
        , button [ type_ "submit" ]
            [ text "Submit" ]
        ]


chooseView model =
    case model.location.hash of
        "" ->
            loginForm model

        "#mood" ->
            recordMood model

        _ ->
            div [] [ text "you're not supposed to be here" ]

module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onInput)
import Http
import Json.Decode as Decode exposing (..)


main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( initialModel, Cmd.none )
        }



-- Model


type alias Model =
    { emailInput : String
    , feilmelding : String
    }


initialModel : Model
initialModel =
    { emailInput = ""
    , feilmelding = ""
    }


type Msg
    = EmailInputChange String
    | EmailInputKeyPress Int
    | ServerRespons (Result Http.Error String)



-- Update


isEnter key =
    key == 13


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
    Decode.at [ "data", "image_url" ] Decode.string


postEmailToServer email =
    let
        url =
            "http://quister.org/regnsjekk/api/varsler"

        request =
            Http.post url Http.emptyBody decodeGifUrl
    in
    Http.send ServerRespons request


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EmailInputChange inputContent ->
            ( { model | emailInput = inputContent }, Cmd.none )

        EmailInputKeyPress key ->
            if isEnter key then
                ( { model | emailInput = "" }, postEmailToServer model.emailInput )
            else
                ( model, Cmd.none )

        ServerRespons response ->
            case response of
                Ok result ->
                    ( model, Cmd.none )

                Err err ->
                    ( { model | feilmelding = toString err }, Cmd.none )


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)



-- View


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ header []
            [ h1 [] [ text "Sykle til jobben?" ]
            , p [ class "tagline" ] [ i [] [ text "Få daglig regnvarsel!" ] ]
            , p [ class "emoji" ] [ text "☔️" ]
            , input [ type_ "email", placeholder "you@mail.com", onInput EmailInputChange, onKeyDown EmailInputKeyPress, Html.Attributes.value model.emailInput ] [ text "halloja" ]
            , viewFeilmelding model.feilmelding
            ]
        ]


viewFeilmelding : String -> Html msg
viewFeilmelding feilmelding =
    if feilmelding == "" then
        text " "
    else
        div [ class "feilmelding" ] [ text feilmelding ]

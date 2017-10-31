module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode exposing (Value, bool, encode, float, int, list, object, string)


main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , init = ( initialModel, Cmd.none )
        }



-- Model


type alias UserInput =
    { sted : String
    , email : String
    }


type alias Model =
    { feilmelding : String
    , lesMer : Bool
    , stage : Stage
    , userInput : UserInput
    }


initialModel : Model
initialModel =
    { feilmelding = ""
    , lesMer = False
    , stage = Email
    , userInput = UserInput "" ""
    }


type Msg
    = ServerRespons (Result Http.Error String)
    | LesMerToggle
    | UserInputEvent UserInputEvent


type UserInputEvent
    = StedInputChange String
    | StedInputKeyPress Int
    | EmailInputChange String
    | EmailInputKeyPress Int


type Stage
    = Email
    | Sted



-- Update


isEnter key =
    key == 13


decodeGifUrl : Decode.Decoder String
decodeGifUrl =
    Decode.at [ "data", "image_url" ] Decode.string


encodeEmail : String -> Json.Encode.Value
encodeEmail email =
    Json.Encode.object [ ( "email", Json.Encode.string email ) ]


postEmailToServer : String -> Cmd Msg
postEmailToServer email =
    let
        url =
            "https://ebrec20i63.execute-api.eu-central-1.amazonaws.com/prod/regnsjekk-users-service/users"

        request =
            Http.post url (Http.jsonBody (encodeEmail email)) decodeGifUrl
    in
    Http.send ServerRespons request


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LesMerToggle ->
            ( { model | lesMer = toggle model.lesMer }, Cmd.none )

        UserInputEvent userInputEvent ->
            updateUserInputEvent userInputEvent model

        ServerRespons response ->
            case response of
                Ok result ->
                    ( { model | stage = Sted }, Cmd.none )

                Err err ->
                    ( { model | feilmelding = toString err }, Cmd.none )


updateUserInputEvent : UserInputEvent -> Model -> ( Model, Cmd Msg )
updateUserInputEvent userInputEvent model =
    case userInputEvent of
        StedInputChange input ->
            ( { model | userInput = UserInput input model.userInput.email }, Cmd.none )

        StedInputKeyPress key ->
            if isEnter key then
                ( model, postEmailToServer model.userInput.email )
            else
                ( { model | feilmelding = "" }, Cmd.none )

        EmailInputChange input ->
            ( { model | userInput = UserInput model.userInput.sted input }, Cmd.none )

        EmailInputKeyPress key ->
            if isEnter key then
                ( { model | stage = Sted }, Cmd.none )
            else
                ( { model | feilmelding = "" }, Cmd.none )


toggle : Bool -> Bool
toggle verdi =
    if verdi == True then
        False
    else
        True


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)



-- View


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ header [] [ h1 [] [ text "Sykle til jobben?" ] ]
        , div [ class "main-content" ]
            [ div [ class "tagline" ]
                [ div [ class "tagline-tekst" ]
                    [ p [] [ i [] [ text "Få daglig regnvarsel!" ] ]
                    , a [ class "lesMerLenke", onClick LesMerToggle ] [ text (viewLesMerTekst model) ]
                    , viewLesMer model
                    ]
                , p [ class "emoji" ] [ text "☔️" ]
                ]
            , div []
                [ viewInput model
                , viewFeilmelding model.feilmelding
                ]
            ]
        ]


viewInput : Model -> Html Msg
viewInput model =
    case model.stage of
        Email ->
            input
                [ type_ "email"
                , onInput (UserInputEvent << EmailInputChange)
                , onKeyDown (UserInputEvent << EmailInputKeyPress)
                , placeholder "you@mail.com"
                , Html.Attributes.value model.userInput.email
                ]
                []

        Sted ->
            div []
                [ span [] [ i [] [ text "Hvor vil du har varsler for?" ] ]
                , span [ class "stage-info" ] [ i [] [ text "2/2" ] ]
                , input
                    [ placeholder "Oslo"
                    , Html.Attributes.value model.userInput.sted
                    , onInput (UserInputEvent << StedInputChange)
                    , onKeyDown (UserInputEvent << StedInputKeyPress)
                    ]
                    []
                ]


viewLesMerTekst : Model -> String
viewLesMerTekst model =
    if model.lesMer == True then
        "Lukk"
    else
        "Les mer"


viewLesMer : Model -> Html msg
viewLesMer model =
    if model.lesMer then
        div [ class "lesMerTekst" ] [ text "Lurer du på om du trenger å ta med regnjakka idag? Hver morgen på angitt tidspunkt sjekker vi værmeldingen for deg - og sender deg varsel om det skal regne når du er på reisefot. " ]
    else
        text ""


viewFeilmelding : String -> Html msg
viewFeilmelding feilmelding =
    if feilmelding == "" then
        text " "
    else
        div [ class "feilmelding" ] [ text feilmelding ]

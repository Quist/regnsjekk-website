module Main exposing (..)

import Dom exposing (focus)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode exposing (Value, bool, encode, float, int, list, object, string)
import Spinner exposing (defaultConfig)
import String exposing (contains)
import Task


main =
    Html.program
        { view = view
        , update = update
        , subscriptions = \model -> Sub.map SpinnerMsg Spinner.subscription
        , init = ( initialModel, Cmd.batch [ Task.attempt FocusResult (focus emailInputHtmlId) ] )
        }



-- Model


stedInputHtmlId =
    "sted-input"


emailInputHtmlId =
    "email-input"


type alias UserInput =
    { sted : String
    , email : String
    }


type alias Model =
    { feilmelding : String
    , feedback : String
    , lesMer : Bool
    , stage : Stage
    , userInput : UserInput
    , spinner : Spinner.Model
    }


initialModel : Model
initialModel =
    { feilmelding = ""
    , feedback = ""
    , lesMer = False
    , stage = Email
    , userInput = UserInput "" ""
    , spinner = Spinner.init
    }


type Msg
    = ServerRespons (Result Http.Error String)
    | LesMerToggle
    | Reset
    | UserInputEvent UserInputEvent
    | SpinnerMsg Spinner.Msg
    | FocusOn String
    | FocusResult (Result Dom.Error ())


type UserInputEvent
    = StedInputChange String
    | StedInputKeyPress Int
    | EmailInputChange String
    | EmailInputKeyPress Int


type Stage
    = Email
    | Sted
    | WaitingForServerResponse
    | SuccessfulSubmit



-- Update


isEnter key =
    key == 13


decodeServerResponse : Decode.Decoder String
decodeServerResponse =
    Decode.at [ "message" ] Decode.string


encodeEmail : String -> Json.Encode.Value
encodeEmail email =
    Json.Encode.object [ ( "email", Json.Encode.string email ) ]


parseError : Http.Error -> String
parseError err =
    case err of
        Http.BadStatus err ->
            err.body

        Http.Timeout ->
            toString err

        Http.BadUrl _ ->
            toString err

        Http.NetworkError ->
            toString err

        Http.BadPayload _ _ ->
            toString err


postEmailToServer : String -> Cmd Msg
postEmailToServer email =
    let
        url =
            "https://ebrec20i63.execute-api.eu-central-1.amazonaws.com/prod/regnsjekk-users-service/users"

        request =
            Http.post url (Http.jsonBody (encodeEmail email)) decodeServerResponse
    in
        Http.send ServerRespons request


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LesMerToggle ->
            ( { model | lesMer = toggle model.lesMer }, Cmd.none )

        FocusOn id ->
            ( model, Cmd.batch [ Task.attempt FocusResult (focus id) ] )

        FocusResult result ->
            ( model, Cmd.none )

        UserInputEvent userInputEvent ->
            updateUserInputEvent userInputEvent model

        Reset ->
            ( initialModel, Cmd.none )

        SpinnerMsg msg ->
            let
                spinnerModel =
                    Spinner.update msg model.spinner
            in
                { model | spinner = spinnerModel } ! []

        ServerRespons response ->
            case response of
                Ok result ->
                    ( { model | feedback = "Flott! Du er påmeldt 💪", stage = SuccessfulSubmit }, Cmd.none )

                Err err ->
                    ( { model | feilmelding = parseError err, stage = Email }, Cmd.none )


updateUserInputEvent : UserInputEvent -> Model -> ( Model, Cmd Msg )
updateUserInputEvent userInputEvent model =
    case userInputEvent of
        StedInputChange input ->
            ( { model | userInput = UserInput input model.userInput.email }, Cmd.none )

        StedInputKeyPress key ->
            if isEnter key then
                ( { model | stage = WaitingForServerResponse }, Cmd.batch [ postEmailToServer model.userInput.email ] )
            else
                ( { model | feilmelding = "" }, Cmd.none )

        EmailInputChange input ->
            ( { model | userInput = UserInput model.userInput.sted input }, Cmd.none )

        EmailInputKeyPress key ->
            if isEnter key then
                if isValidEmail model.userInput.email then
                    ( { model | stage = Sted }, Cmd.batch [ Task.attempt FocusResult (focus stedInputHtmlId) ] )
                else
                    ( { model | feilmelding = "Invalid email" }, Cmd.none )
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


isValidEmail : String -> Bool
isValidEmail email =
    contains "@" email



-- View


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ header [] [ h1 [] [ a [ onClick Reset ] [ text "Sykle til jobben?" ] ] ]
        , div [ class "main-content" ]
            [ div [ class "tagline" ]
                [ div [ class "tagline-tekst" ]
                    [ p [] [ i [] [ text "Få regnvarsel hver morgen!" ] ]
                    ]
                , p [ class "emoji" ] [ text "☔️" ]
                ]
            , div []
                [ viewInput model
                , viewFeilmelding model.feilmelding
                , viewFeedback model.feedback
                , a [ class "lesMerLenke", onClick LesMerToggle ] [ text (viewLesMerTekst model) ]
                , viewLesMer model
                ]
            ]
        ]


viewInput : Model -> Html Msg
viewInput model =
    case model.stage of
        Email ->
            input
                [ id emailInputHtmlId
                , type_ "email"
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
                    [ id stedInputHtmlId
                    , placeholder "Oslo"
                    , Html.Attributes.value model.userInput.sted
                    , onInput (UserInputEvent << StedInputChange)
                    , onKeyDown (UserInputEvent << StedInputKeyPress)
                    ]
                    []
                ]

        WaitingForServerResponse ->
            let
                config =
                    { defaultConfig | scale = 0.75 }
            in
                div [] [ Spinner.view config model.spinner ]

        SuccessfulSubmit ->
            text ""


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
        div [ class "feilmelding infoboks" ] [ text feilmelding ]


viewFeedback : String -> Html msg
viewFeedback feedback =
    if feedback == "" then
        text " "
    else
        div [ class "feedback infoboks" ] [ text feedback ]

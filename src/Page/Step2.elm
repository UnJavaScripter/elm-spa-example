module Page.Step2 exposing (ExternalMsg(..), Model, Msg, initialModel, update, view)

{-| The Step2 page.
-}

import Data.Session exposing (Session)
import Data.User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (decode, optional)
import Request.User exposing (storeSession)
import Route exposing (Route)
import Util exposing ((=>))
import Validate exposing (Validator, ifBlank, validate)
import Views.Form as Form


-- MODEL --


type alias Model =
    { errors : List Error
    , email : String
    , password : String
    , algo : String
    }


initialModel : Model
initialModel =
    { errors = []
    , email = ""
    , password = ""
    , algo = "chaaaaaaaa"
    }



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "auth-page" ]
        [ 
            div[] [text "Step2 page"]
        ]




-- UPDATE --


type Msg
    = T


type ExternalMsg
    = NoOp
    | SetUser User


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        T ->
            model 
                => Cmd.none
                => NoOp

-- VALIDATION --


type Field
    = Form
    | Email
    | Password


{-| Recording validation errors on a per-field basis facilitates displaying
them inline next to the field where the error occurred.

I implemented it this way out of habit, then realized the spec called for
displaying all the errors at the top. I thought about simplifying it, but then
figured it'd be useful to show how I would normally model this data - assuming
the intended UX was to render errors per field.

(The other part of this is having a view function like this:

viewFormErrors : Field -> List Error -> Html msg

...and it filters the list of errors to render only the ones for the given
Field. This way you can call this:

viewFormErrors Email model.errors

...next to the `email` field, and call `viewFormErrors Password model.errors`
next to the `password` field, and so on.

-}
type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .email (Email => "email can't be blank.")
        , ifBlank .password (Password => "password can't be blank.")
        ]


errorsDecoder : Decoder (List String)
errorsDecoder =
    decode (\emailOrPassword email username password -> List.concat [ emailOrPassword, email, username, password ])
        |> optionalError "email or password"
        |> optionalError "email"
        |> optionalError "username"
        |> optionalError "password"


optionalError : String -> Decoder (List String -> a) -> Decoder a
optionalError fieldName =
    let
        errorToString errorMessage =
            String.join " " [ fieldName, errorMessage ]
    in
    optional fieldName (Decode.list (Decode.map errorToString string)) []

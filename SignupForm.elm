module SignupForm where

import Http
import Effects
import StartApp
import Html exposing (..)
import Task exposing (Task)
import Html.Events exposing (..)
import Json.Decode exposing (succeed)
import Html.Attributes exposing (id, type', for, value, class)


-- MODEL

type alias Errors =
  { username : String
  , password : String
  , usernameTaken : Bool
  }


type alias Model =
  { username : String
  , password : String
  , errors : Errors
  }


initialErrors : Errors
initialErrors =
  { username = ""
  , password = ""
  , usernameTaken = False
  }


initialModel : Model
initialModel =
  { username = ""
  , password = ""
  , errors = initialErrors
  }


-- UPDATE

type Action
  = Validate
  | SetUsername String
  | SetPassword String
  | UsernameTaken
  | UsernameAvailable


getErrors : Model -> Errors
getErrors model =
  { username =
      if model.username == "" then
        "Please enter a username!"
      else
        ""
  , password =
      if model.password == "" then
        "Please enter a password!"
      else
        ""
  , usernameTaken =
      model.errors.usernameTaken
  }


withUsernameTaken : Bool -> Model -> Model
withUsernameTaken isTaken model =
  let
    currentErrors =
      model.errors

    newErrors =
      { currentErrors | usernameTaken = isTaken }
  in
    { model | errors = newErrors }


update : Action -> Model -> ( Model, Effects.Effects Action )
update action model =
  case action of
    Validate ->
      let
        url =
          "https://api.github.com/users/" ++ model.username

        request =
          Http.get (succeed UsernameTaken) url

        safeRequest =
          Task.onError request (\_ -> Task.succeed UsernameAvailable)
      in
        ( { model | errors = getErrors model }, Effects.task safeRequest )

    SetUsername username ->
      ( { model | username = username }, Effects.none )

    SetPassword password ->
      ( { model | password = password }, Effects.none )

    UsernameAvailable ->
      ( withUsernameTaken False model, Effects.none )

    UsernameTaken ->
      ( withUsernameTaken True model, Effects.none )


-- VIEW

viewUsernameErrors : Model -> String
viewUsernameErrors model =
  if model.errors.usernameTaken then
    "That username is taken!"
  else
    model.errors.username


view : Signal.Address Action -> Model -> Html
view address model =
  form
    [ id "signup-form" ]
    [ h1 [] [ text "Sensational Signup Form" ]
    , label [ for "username-field" ] [ text "username: " ]
    , input
        [ id "username-field"
        , type' "text"
        , value model.username
        , on "input" targetValue (\str -> Signal.message address (SetUsername str))
        ]
        []
    , div [ class "validation-error" ] [ text model.errors.username ]
    , label [ for "password"] [text "password: " ]
    , input
        [ id "password-field"
        , type' "password"
        , value model.password
        , on "input" targetValue (\str -> Signal.message address (SetPassword str))
        ]
        []
    , div [ class "validation-error" ] [ text model.errors.password ]
    , div [ class "signup-button", onClick address Validate ] [ text "Sign Up!" ]
    , div [ class "validation-error" ] [ text (viewUsernameErrors model) ]
    ]


app =
  StartApp.start
    { init = (initialModel, Effects.none)
    , update = update
    , view = view
    , inputs = []
    }


main =
  app.html


port tasks : Signal (Task Effects.Never ())
port tasks =
  app.tasks

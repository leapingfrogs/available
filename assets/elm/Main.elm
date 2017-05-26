module App exposing (..)

import Dict as Dict exposing (keys)
import Html exposing (Html, br, button, div, fieldset, img, input, label, option, select, span, text)
import Html.Attributes exposing (class, classList, for, height, name, placeholder, src, type_, value, width)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Http
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode as Encode
import List.Extra as List
import MD5
import Phoenix.Channel
import Phoenix.Push
import Phoenix.Socket
import Ports.LocalStorage as LocalStorage
import Time exposing (Time)
import Time.DateTime as DateTime exposing (DateTime)
import Time.TimeZone as TimeZone exposing (TimeZone, name)
import Time.TimeZones as TimeZones
import Time.ZonedDateTime as ZonedDateTime exposing (ZonedDateTime)


---- MODEL ----


apiRoot : String
apiRoot =
    "http://localhost:4000/api"


webSocket : String
webSocket =
    "ws://localhost:4000/socket/websocket"


avatarSize : Int
avatarSize =
    100


avatarType : String
avatarType =
    "identicon"


type View
    = LoginView LoginModel
    | SignupView SignupModel
    | ColleaguesView ColleagueModel


type alias Model =
    { currentView : View
    , currentUser : Maybe Person
    , now : Maybe DateTime
    , phxSocket : Phoenix.Socket.Socket Msg
    }


type alias LoginModel =
    { email : String
    , password : String
    , error : String
    }


type alias SignupModel =
    { email : String
    , location : String
    , name : String
    , timezone : String
    , error : String
    }


type alias ColleagueModel =
    { colleagues : List Person }


type alias Person =
    { name : String
    , email : String
    , location : String
    , timezone : String
    , workingHours : WorkingHours
    , displayName : Bool
    }


type alias WorkingHours =
    { timezone : String
    , blocks : List TimeBlock
    }


type alias TimeBlock =
    { start : SimpleTime, end : SimpleTime }


type alias SimpleTime =
    { hour : Int, minute : Int }


type alias ColleagueList =
    { colleagues : List Person }


colleagueListDecoder : Decoder ColleagueList
colleagueListDecoder =
    decode ColleagueList
        |> required "colleagues" (list personDecoder)


personDecoder : Decoder Person
personDecoder =
    decode Person
        |> required "name" string
        |> required "email" string
        |> required "location" string
        |> required "timezone" string
        |> optional "workingHours" workingHoursDecoder (WorkingHours "Utc" [])
        |> hardcoded False


workingHoursDecoder : Decoder WorkingHours
workingHoursDecoder =
    decode WorkingHours
        |> required "timezone" string
        |> required "blocks" (list timeBlockDecoder)


timeBlockDecoder : Decoder TimeBlock
timeBlockDecoder =
    decode TimeBlock
        |> required "start" simpleTimeDecoder
        |> required "end" simpleTimeDecoder


simpleTimeDecoder : Decoder SimpleTime
simpleTimeDecoder =
    decode SimpleTime
        |> required "hour" int
        |> required "minute" int


demoColleagues : List Person
demoColleagues =
    [ { name = "One User"
      , email = "one@flubber.com"
      , location = "Paris, France"
      , timezone = "Europe/Paris"
      , workingHours =
            { timezone =
                "Europe/Paris"
            , blocks =
                [ { start = SimpleTime 19 1, end = SimpleTime 20 0 }
                ]
            }
      , displayName = False
      }
    , { name = "Two User"
      , email = "two@flubber.com"
      , location = "Zurich, Switzerland"
      , timezone = "Europe/Zurich"
      , workingHours =
            { timezone =
                "Europe/Zurich"
            , blocks =
                [ { start = SimpleTime 8 0, end = SimpleTime 18 0 }
                ]
            }
      , displayName = False
      }
    , { name = "Three User"
      , email = "three@flubber.com"
      , location = "Berlin, Germany"
      , timezone = "Europe/Berlin"
      , workingHours =
            { timezone =
                "Europe/Berlin"
            , blocks =
                [ { start = SimpleTime 7 0, end = SimpleTime 20 0 }
                ]
            }
      , displayName = False
      }
    , { name = "Four User"
      , email = "four@flubber.com"
      , location = "San Francisco, CA"
      , timezone = "America/Los_Angeles"
      , workingHours =
            { timezone =
                "America/Los_Angeles"
            , blocks =
                [ { start = SimpleTime 7 0, end = SimpleTime 20 0 }
                ]
            }
      , displayName = False
      }
    , { name = "Five User"
      , email = "five@flubber.com"
      , location = "San Francisco, CA"
      , timezone = "America/Los_Angeles"
      , workingHours =
            { timezone =
                "America/Los_Angeles"
            , blocks =
                [ { start = SimpleTime 9 0, end = SimpleTime 18 0 }
                ]
            }
      , displayName = False
      }
    ]


initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init webSocket
        |> Phoenix.Socket.withDebug


initModel : Model
initModel =
    { phxSocket = initPhxSocket
    , now = Nothing
    , currentUser = Nothing
    , currentView = LoginView (LoginModel "" "" "")
    }



---- UPDATE ----


type LoginField
    = LoginEmail
    | LoginPassword


type SignupField
    = SignupEmail
    | SignupLocation
    | SignupName
    | SignupTimezone


type Msg
    = ShowDetail Bool Person
    | Tick Time
    | LoadData Encode.Value
    | AddColleague Encode.Value
    | Joined String
    | Closed String
    | UpdateLogin LoginField String
    | UpdateSignup SignupField String
    | Login
    | SignUp
    | SelectView View
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | HandleLoginRequest (Result Http.Error Person)
    | HandleSignupRequest (Result Http.Error Person)
    | ReceiveFromLocalStorage ( LocalStorage.Key, LocalStorage.Value )
    | Logout


join : String -> Phoenix.Channel.Channel Msg
join channel =
    Phoenix.Channel.init channel
        |> Phoenix.Channel.withPayload (Encode.object [])
        |> Phoenix.Channel.onJoin (always (Joined channel))
        |> Phoenix.Channel.onClose (always (Closed channel))


connect : Model -> ( Model, Cmd Msg )
connect model =
    case model.currentUser of
        Nothing ->
            model ! []

        Just user ->
            let
                companyChannel =
                    join <| "user:all"

                channel =
                    join <| "user:" ++ user.email

                ( phxSocketCompany, phxCmdCompany ) =
                    model.phxSocket
                        |> Phoenix.Socket.on "data" ("user:" ++ user.email) LoadData
                        |> Phoenix.Socket.join companyChannel

                ( phxSocketUser, phxCmdUser ) =
                    phxSocketCompany
                        |> Phoenix.Socket.on "new:colleague" "user:all" AddColleague
                        |> Phoenix.Socket.join channel
            in
            { model | phxSocket = phxSocketUser } ! [ Cmd.map PhoenixMsg phxCmdCompany, Cmd.map PhoenixMsg phxCmdUser, LocalStorage.storageSetItem ( "currentUser", Encode.string user.email ) ]


findUser : Model -> ( Model, Cmd Msg )
findUser model =
    model ! [ LocalStorage.storageGetItem "currentUser" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Logout ->
            let
                ( phxSocket, phxCmd ) =
                    case model.currentUser of
                        Just user ->
                            Phoenix.Socket.leave ("user:" ++ user.email) model.phxSocket

                        Nothing ->
                            ( model.phxSocket, Cmd.none )
            in
            { model
                | currentView = LoginView { email = "", password = "", error = "" }
                , currentUser = Nothing
                , phxSocket = phxSocket
            }
                ! [ Cmd.map PhoenixMsg phxCmd, LocalStorage.storageClear () ]

        Tick time ->
            { model | now = Just (DateTime.fromTimestamp time) } ! []

        ShowDetail show person ->
            case model.currentView of
                ColleaguesView colleaguesModel ->
                    { model
                        | currentView =
                            ColleaguesView
                                { colleagues =
                                    List.map
                                        (\colleague ->
                                            if person == colleague then
                                                { colleague | displayName = show }
                                            else
                                                colleague
                                        )
                                        colleaguesModel.colleagues
                                }
                    }
                        ! []

                _ ->
                    model ! []

        LoadData raw ->
            case model.currentView of
                ColleaguesView colleaguesModel ->
                    case Decode.decodeValue colleagueListDecoder raw of
                        Ok colleagueData ->
                            { model | currentView = ColleaguesView colleagueData } ! []

                        Err error ->
                            { model | currentView = ColleaguesView { colleagues = demoColleagues } } ! []

                _ ->
                    model ! []

        AddColleague raw ->
            case model.currentView of
                ColleaguesView colleaguesModel ->
                    case Decode.decodeValue colleagueListDecoder raw of
                        Ok colleagueData ->
                            { model | currentView = ColleaguesView { colleagues = colleaguesModel.colleagues ++ colleagueData.colleagues } } ! []

                        Err error ->
                            { model | currentView = ColleaguesView { colleagues = demoColleagues } } ! []

                _ ->
                    model ! []

        Joined "user:all" ->
            let
                debug =
                    Debug.log "Joined Channel" "user:all"
            in
            case model.currentUser of
                Nothing ->
                    model ! []

                Just user ->
                    model
                        ! []

        Joined userChannel ->
            let
                debug =
                    Debug.log "Joined Channel" userChannel
            in
            case model.currentUser of
                Nothing ->
                    model ! []

                Just user ->
                    let
                        payload =
                            Encode.object []

                        push_ =
                            Phoenix.Push.init "load:data" userChannel
                                |> Phoenix.Push.withPayload payload

                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.push push_ model.phxSocket
                    in
                    { model
                        | phxSocket = phxSocket
                    }
                        ! [ Cmd.map PhoenixMsg phxCmd ]

        Closed channel ->
            let
                debug =
                    Debug.log "Closed Channel" channel
            in
            model ! []

        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
            { model | phxSocket = phxSocket }
                ! [ Cmd.map PhoenixMsg phxCmd ]

        SignUp ->
            case model.currentView of
                SignupView signupModel ->
                    model ! [ performSignupRequest signupModel ]

                _ ->
                    model ! []

        Login ->
            case model.currentView of
                LoginView loginModel ->
                    model ! [ performLoginRequest loginModel ]

                _ ->
                    model ! []

        SelectView view ->
            { model
                | currentView = view
            }
                ! []

        HandleLoginRequest (Ok user) ->
            connect
                { model
                    | currentUser = Just user
                    , currentView = ColleaguesView { colleagues = [] }
                }

        HandleLoginRequest (Err message) ->
            case model.currentView of
                LoginView { email, password, error } ->
                    { model
                        | currentView = LoginView { email = email, password = password, error = "Login failed" }
                    }
                        ! []

                _ ->
                    model ! []

        HandleSignupRequest (Ok user) ->
            connect
                { model
                    | currentUser = Just user
                    , currentView = ColleaguesView { colleagues = [] }
                }

        HandleSignupRequest (Err message) ->
            case model.currentView of
                SignupView { email, location, name, timezone, error } ->
                    { model
                        | currentView = SignupView { email = email, location = location, name = name, timezone = timezone, error = "Signup failed" }
                    }
                        ! []

                _ ->
                    model ! []

        UpdateLogin field value ->
            case model.currentView of
                LoginView { email, password, error } ->
                    case field of
                        LoginEmail ->
                            { model
                                | currentView = LoginView { email = value, password = password, error = error }
                            }
                                ! []

                        LoginPassword ->
                            { model
                                | currentView = LoginView { email = email, password = value, error = error }
                            }
                                ! []

                _ ->
                    model ! []

        UpdateSignup field value ->
            case model.currentView of
                SignupView { email, location, name, timezone, error } ->
                    case field of
                        SignupEmail ->
                            { model
                                | currentView = SignupView { email = value, location = location, name = name, timezone = timezone, error = error }
                            }
                                ! []

                        SignupLocation ->
                            { model
                                | currentView = SignupView { email = email, location = value, name = name, timezone = timezone, error = error }
                            }
                                ! []

                        SignupName ->
                            { model
                                | currentView = SignupView { email = email, location = location, name = value, timezone = timezone, error = error }
                            }
                                ! []

                        SignupTimezone ->
                            { model
                                | currentView = SignupView { email = email, location = location, name = name, timezone = value, error = error }
                            }
                                ! []

                _ ->
                    model ! []

        ReceiveFromLocalStorage ( "currentUser", jsonValue ) ->
            case Decode.decodeValue Decode.string jsonValue of
                Ok value ->
                    let
                        loginModel =
                            { email = value, password = "", error = "" }
                    in
                    { model | currentView = LoginView loginModel }
                        ! [ performLoginRequest loginModel ]

                Err value ->
                    model ! []

        ReceiveFromLocalStorage ( key, jsonValue ) ->
            model ! []


apiUrl : String -> String
apiUrl action =
    apiRoot ++ action


performLoginRequest : LoginModel -> Cmd Msg
performLoginRequest loginForm =
    let
        url =
            apiUrl "/login"

        request =
            Http.post url
                (Http.jsonBody <|
                    loginJson
                        { username = loginForm.email
                        , password = loginForm.password
                        }
                )
                personDecoder
    in
    Http.send HandleLoginRequest request


performSignupRequest : SignupModel -> Cmd Msg
performSignupRequest signup =
    let
        url =
            apiUrl "/signup"

        request =
            Http.post url
                (Http.jsonBody <|
                    signupJson
                        { email = signup.email
                        , location = signup.location
                        , name = signup.name
                        , timezone = signup.timezone
                        }
                )
                personDecoder
    in
    Http.send HandleSignupRequest request


loginJson : { username : String, password : String } -> Encode.Value
loginJson details =
    Encode.object [ ( "username", Encode.string details.username ), ( "password", Encode.string details.password ) ]


signupJson : { email : String, location : String, name : String, timezone : String } -> Encode.Value
signupJson details =
    Encode.object
        [ ( "email", Encode.string details.email )
        , ( "location", Encode.string details.location )
        , ( "name", Encode.string details.name )
        , ( "timezone", Encode.string details.timezone )
        ]



---- VIEW ----


type alias ViewModel =
    { userName : String
    , userEmail : String
    , userTimezone : TimeZone
    , localtime : ZonedDateTime
    , timezones : List Resolved
    }


signupView : SignupModel -> Html Msg
signupView model =
    div [ class "app" ]
        [ div [ class "header" ]
            [ div [ class "title" ] []
            ]
        , div [ class "content" ]
            [ div [ class "login" ]
                [ fieldset []
                    [ div []
                        [ label [ for "email" ] [ text "Email" ]
                        , input [ onInput (UpdateSignup SignupEmail), type_ "text", Html.Attributes.name "email", Html.Attributes.value model.email, placeholder "someone@somewhere.com" ] []
                        ]
                    , div []
                        [ label [ for "name" ] [ text "Name" ]
                        , input [ onInput (UpdateSignup SignupName), type_ "text", Html.Attributes.name "name", Html.Attributes.value model.name, placeholder "Full Name" ] []
                        ]
                    , div []
                        [ label [ for "location" ] [ text "Location" ]
                        , input [ onInput (UpdateSignup SignupLocation), type_ "text", Html.Attributes.name "location", Html.Attributes.value model.location, placeholder "location" ] []
                        ]
                    , div []
                        [ label [ for "timezone" ] [ text "Timezone" ]
                        , select [ onInput (UpdateSignup SignupTimezone), Html.Attributes.name "timezone" ]
                            (let
                                timezones =
                                    TimeZones.all
                                        |> Dict.keys
                             in
                             List.map
                                (\timezone -> option [] [ text timezone ])
                                timezones
                            )
                        ]
                    , div [ class "error" ]
                        [ text model.error ]
                    , div []
                        [ button [ onClick (SelectView (LoginView (LoginModel model.email "" ""))) ] [ text "Login" ]
                        , button [ onClick SignUp ] [ text "Sign Up" ]
                        ]
                    ]
                ]
            ]
        ]


loginView : LoginModel -> Html Msg
loginView model =
    div [ class "app" ]
        [ div [ class "header" ]
            [ div [ class "title" ] []
            ]
        , div [ class "content" ]
            [ div [ class "login" ]
                [ fieldset []
                    [ div []
                        [ label [ for "email" ] [ text "Email" ]
                        , input [ onInput (UpdateLogin LoginEmail), type_ "text", Html.Attributes.name "email", Html.Attributes.value model.email, placeholder "someone@somewhere.com" ] []
                        ]
                    , div []
                        [ label [ for "password" ] [ text "Password" ]
                        , input [ onInput (UpdateLogin LoginPassword), type_ "password", Html.Attributes.name "password", Html.Attributes.value model.password, placeholder "password" ] []
                        ]
                    , div [ class "error" ]
                        [ text model.error ]
                    , div []
                        [ button [ onClick Login ] [ text "Login" ]
                        , button [ onClick (SelectView (SignupView (SignupModel model.email "" "" "" ""))) ] [ text "Sign Up" ]
                        ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    case model.currentView of
        LoginView loginModel ->
            loginView loginModel

        SignupView signupModel ->
            signupView signupModel

        ColleaguesView colleagueModel ->
            colleagueView model colleagueModel


colleagueView : Model -> ColleagueModel -> Html Msg
colleagueView model colleagueModel =
    let
        maybeViewModel =
            modelToView model
    in
    case maybeViewModel of
        Nothing ->
            div [] []

        Just viewModel ->
            let
                offsets =
                    viewModel.timezones
                        |> List.map resolvedTimezone
                        |> List.uniqueBy (TimeZone.abbreviation (ZonedDateTime.toTimestamp viewModel.localtime))
                        |> List.sortWith timezoneComparison
            in
            div [ class "app" ]
                [ div [ class "header" ]
                    [ div [ class "title" ]
                        [ div [] [ text "Who's Available?" ]
                        ]
                    , div [ class "time" ] [ text (formatTimeZone viewModel.localtime viewModel.userTimezone) ]
                    , div [ class "user", onClick Logout ]
                        [ div [] [ text viewModel.userName ]
                        , img [ src (gravatarUrl viewModel.userEmail), width 30, height 30 ] []
                        ]
                    ]
                , div [ class "content" ]
                    [ div [ class "timezones" ]
                        (List.map
                            (zoneColumn viewModel.localtime colleagueModel.colleagues)
                            offsets
                        )
                    ]
                ]


zoneColumn : ZonedDateTime -> List Person -> TimeZone -> Html Msg
zoneColumn time colleagues zone =
    let
        people =
            colleagues
                |> List.filter (\colleague -> TimeZone.abbreviation (ZonedDateTime.toTimestamp time) zone == TimeZone.abbreviation (ZonedDateTime.toTimestamp time) (resolvedTimezone (resolveTimezone colleague.timezone)))
    in
    div [ class "zone" ]
        ([ div [ class "timezone" ]
            [ text (formatTimeZone time zone)
            ]
         ]
            ++ List.map (person time) people
        )


person : ZonedDateTime -> Person -> Html Msg
person time person =
    let
        theirTimezone =
            resolvedTimezone (resolveTimezone person.workingHours.timezone)

        nowInTheirTime =
            convertTime theirTimezone time

        nowTimeTuple =
            ( ZonedDateTime.hour nowInTheirTime, ZonedDateTime.minute nowInTheirTime )

        userIntervals =
            person.workingHours.blocks

        working =
            List.any
                (\block ->
                    let
                        startWork =
                            block.start

                        endWork =
                            block.end

                        ( nowHour, nowMinute ) =
                            nowTimeTuple

                        afterStart =
                            nowHour > startWork.hour || (nowHour == startWork.hour && nowMinute >= startWork.minute)

                        beforeEnd =
                            nowHour < endWork.hour || (nowHour == endWork.hour && nowMinute < endWork.minute)
                    in
                    afterStart && beforeEnd
                )
                userIntervals

        meeting =
            person.name == "Richard Feldman"
    in
    div
        [ classList
            [ ( "person", True )
            , ( "not_working", not working )
            , ( "available", working )
            , ( "meeting", meeting )
            , ( "shown", person.displayName )
            ]
        ]
        [ img [ onMouseEnter (ShowDetail True person), onMouseLeave (ShowDetail False person), src (gravatarUrl person.email), width avatarSize, height avatarSize ] []
        , div
            [ class "name"
            ]
            [ text person.name
            , br [] []
            , text <| "(" ++ person.location ++ ")"
            ]
        ]


convertTime : TimeZone -> ZonedDateTime -> ZonedDateTime
convertTime timezone time =
    ZonedDateTime.fromDateTime timezone (ZonedDateTime.toDateTime time)


timezoneOffset : TimeZone -> Int
timezoneOffset zone =
    TimeZone.offset 0.0 zone


timezoneComparison : TimeZone -> TimeZone -> Order
timezoneComparison zone1 zone2 =
    let
        offset1 =
            TimeZone.offset 0.0 zone1

        offset2 =
            TimeZone.offset 0.0 zone2
    in
    case compare offset1 offset2 of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


formatTimeZone : ZonedDateTime -> TimeZone -> String
formatTimeZone time timezone =
    let
        localtime =
            convertTime timezone time
    in
    [ toString (ZonedDateTime.hour localtime)
    , ":"
    , String.padLeft 2 '0' (toString (ZonedDateTime.minute localtime))
    , " "
    , ZonedDateTime.abbreviation localtime
    ]
        |> List.foldr (++) ""


modelToView : Model -> Maybe ViewModel
modelToView model =
    case model.currentView of
        ColleaguesView colleaguesModel ->
            case model.currentUser of
                Nothing ->
                    Nothing

                Just user ->
                    let
                        timezones =
                            colleaguesModel.colleagues
                                |> List.map .timezone
                                |> List.map resolveTimezone

                        userTimezone =
                            resolvedTimezone (resolveTimezone user.timezone)
                    in
                    Just
                        { userName = user.name
                        , userEmail = user.email
                        , userTimezone = userTimezone
                        , localtime = localtime userTimezone model.now
                        , timezones = timezones
                        }

        _ ->
            Nothing


localtime : TimeZone -> Maybe DateTime -> ZonedDateTime
localtime timezone datetime =
    case datetime of
        Nothing ->
            ZonedDateTime.zonedDateTime timezone ZonedDateTime.zero

        Just datetime ->
            ZonedDateTime.fromDateTime timezone datetime


type Resolved
    = User TimeZone
    | Default TimeZone


resolveTimezone : String -> Resolved
resolveTimezone name =
    case TimeZones.fromName name of
        Nothing ->
            Default (TimeZones.utc ())

        Just zone ->
            User zone


resolvedTimezone : Resolved -> TimeZone
resolvedTimezone resolved =
    case resolved of
        User timezone ->
            timezone

        Default timezone ->
            timezone


gravatarHash : String -> String
gravatarHash email =
    email
        |> String.trim
        |> String.toLower
        |> MD5.hex


gravatarUrl : String -> String
gravatarUrl email =
    "https://www.gravatar.com/avatar/" ++ gravatarHash email ++ "?d=" ++ avatarType ++ "&s=" ++ toString avatarSize



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second Tick
        , Phoenix.Socket.listen model.phxSocket PhoenixMsg
        , LocalStorage.storageGetItemResponse ReceiveFromLocalStorage
        ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = findUser initModel
        , update = update
        , subscriptions = subscriptions
        }

module App exposing (..)

import Html exposing (Html, text, div, span, img, br, button, fieldset, input, label)
import Html.Attributes exposing (class, classList, src, width, height, type_, value, placeholder, for, name)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave, onInput)
import List.Extra as List
import MD5
import Time exposing (Time)
import Time.DateTime as DateTime exposing (DateTime)
import Time.TimeZone as TimeZone exposing (TimeZone, name)
import Time.TimeZones as TimeZones
import Time.ZonedDateTime as ZonedDateTime exposing (ZonedDateTime)
import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push
import Json.Encode as Encode
import Http
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


---- MODEL ----


avatarSize : Int
avatarSize =
    100


avatarType : String
avatarType =
    "identicon"


type alias Model =
    { user : Maybe Person
    , now : Maybe DateTime
    , colleagues : List Person
    , phxSocket : Phoenix.Socket.Socket Msg
    , connected : Bool
    , loginEmail : String
    , loginPassword : String
    , loginError : String
    }


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
        |> required "workingHours" workingHoursDecoder
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
                [ { start = (SimpleTime 19 1), end = (SimpleTime 20 0) }
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
                [ { start = (SimpleTime 8 0), end = (SimpleTime 18 0) }
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
                [ { start = (SimpleTime 7 0), end = (SimpleTime 20 0) }
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
                [ { start = (SimpleTime 7 0), end = (SimpleTime 20 0) }
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
                [ { start = (SimpleTime 9 0), end = (SimpleTime 18 0) }
                ]
            }
      , displayName = False
      }
    ]


initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init "ws://localhost:4000/socket/websocket"
        |> Phoenix.Socket.withDebug


initModel : Model
initModel =
    let
        user =
            Nothing
    in
        { phxSocket = initPhxSocket
        , connected = False
        , now = Nothing
        , user = user
        , colleagues = []
        , loginEmail = ""
        , loginPassword = ""
        , loginError = ""
        }



---- UPDATE ----


type LoginField
    = Email
    | Password


type Msg
    = NoOp
    | ShowDetail Bool Person
    | Tick Time
    | LoadData Encode.Value
    | ToggleChannel Bool
    | Joined
    | Closed
    | UpdateLogin LoginField String
    | Login
    | SignUp
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | HandleLogin (Result Http.Error Person)


connect : Model -> ( Model, Cmd Msg )
connect model =
    case model.user of
        Nothing ->
            ( model, Cmd.none )

        Just user ->
            let
                channel =
                    Phoenix.Channel.init (Debug.log "joining: " ("user:" ++ user.email))
                        |> Phoenix.Channel.withPayload (Encode.object [])
                        |> Phoenix.Channel.onJoin (always Joined)
                        |> Phoenix.Channel.onClose (always Closed)

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.join channel model.phxSocket
            in
                ( { model | phxSocket = phxSocket }, Cmd.map PhoenixMsg phxCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | now = Just (DateTime.fromTimestamp time) }, Cmd.none )

        ShowDetail show person ->
            ( { model
                | colleagues =
                    List.map
                        (\colleague ->
                            if person == colleague then
                                { colleague | displayName = show }
                            else
                                colleague
                        )
                        model.colleagues
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        LoadData raw ->
            case Decode.decodeValue colleagueListDecoder raw of
                Ok colleagueData ->
                    ( { model | colleagues = colleagueData.colleagues }, Cmd.none )

                Err error ->
                    ( { model | colleagues = demoColleagues }, Cmd.none )

        ToggleChannel connected ->
            case model.user of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    case connected of
                        True ->
                            let
                                ( phxSocket, phxCmd ) =
                                    Phoenix.Socket.leave ("user:" ++ user.email) model.phxSocket
                            in
                                ( { model | phxSocket = phxSocket }
                                , Cmd.map PhoenixMsg phxCmd
                                )

                        False ->
                            connect model

        Joined ->
            case model.user of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    let
                        payload =
                            (Encode.object [])

                        push_ =
                            Phoenix.Push.init "load:data" ("user:" ++ user.email)
                                |> Phoenix.Push.withPayload payload

                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.push push_ model.phxSocket
                    in
                        ( { model
                            | connected = True
                            , phxSocket = phxSocket
                          }
                        , Cmd.map PhoenixMsg phxCmd
                        )

        Closed ->
            ( { model | colleagues = demoColleagues, connected = False }, Cmd.none )

        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )

        UpdateLogin loginField value ->
            case loginField of
                Email ->
                    ( { model | loginEmail = value }, Cmd.none )

                Password ->
                    ( { model | loginPassword = value }, Cmd.none )

        SignUp ->
            ( model, performRequest SignUpOperation model.loginEmail model.loginPassword )

        Login ->
            --            let
            --                user =
            --                    { name = "Ian Davies"
            --                    , email = model.loginEmail
            --                    , location = "Stirling, Scotland"
            --                    , timezone = "Europe/London"
            --                    , workingHours =
            --                        { timezone = "Europe/London"
            --                        , blocks =
            --                            [ { start = (SimpleTime 9 30), end = (SimpleTime 14 30) }
            --                            , { start = (SimpleTime 15 0), end = (SimpleTime 18 1) }
            --                            ]
            --                        }
            --                    , displayName = False
            --                    }
            --            in
            ( model, performRequest LoginOperation model.loginEmail model.loginPassword )

        HandleLogin result ->
            ( model, Cmd.none )



--                connect
--                    { model
--                        | user =
--                            Just user
--                        , phxSocket =
--                            model.phxSocket |> Phoenix.Socket.on "data" ("user:" ++ user.email) LoadData
--                    }


type RequestOperation
    = LoginOperation
    | SignUpOperation


performRequest : RequestOperation -> String -> String -> Cmd Msg
performRequest operation username password =
    let
        url =
            case operation of
                LoginOperation ->
                    "http://localhost:4000/login"

                SignUpOperation ->
                    "http://localhost:4000/signup"

        request =
            Http.post url (Http.jsonBody <| loginJson { username = username, password = password }) personDecoder
    in
        Http.send HandleLogin request


loginJson : { username : String, password : String } -> Encode.Value
loginJson details =
    Encode.object [ ( "username", Encode.string details.username ), ( "password", Encode.string details.password ) ]



---- VIEW ----


type alias ViewModel =
    { userName : String
    , userEmail : String
    , userTimezone : TimeZone
    , localtime : ZonedDateTime
    , timezones : List Resolved
    }


loginView : Model -> Html Msg
loginView model =
    div [ class "app" ]
        [ div [ class "header" ]
            [ div [ classList [ ( "title", True ), ( "connected", model.connected ) ] ]
                [ div [ onClick (ToggleChannel model.connected) ] [ text "Who's Available?" ]
                ]
            ]
        , div [ class "content" ]
            [ div [ class "login" ]
                [ fieldset []
                    [ div []
                        [ label [ for "email" ] [ text "Email" ]
                        , input [ onInput (UpdateLogin Email), type_ "text", Html.Attributes.name "email", Html.Attributes.value model.loginEmail, placeholder "someone@somewhere.com" ] []
                        ]
                    , div []
                        [ label [ for "password" ] [ text "Password" ]
                        , input [ onInput (UpdateLogin Password), type_ "password", Html.Attributes.name "password", Html.Attributes.value model.loginPassword, placeholder "password" ] []
                        ]
                    , div [ class "error" ]
                        [ text model.loginError ]
                    , div []
                        [ button [ onClick Login ] [ text "Login" ]
                        , button [ onClick SignUp ] [ text "Sign Up" ]
                        ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        maybeViewModel =
            modelToView model
    in
        case maybeViewModel of
            Nothing ->
                loginView model

            Just viewModel ->
                let
                    offsets =
                        viewModel.timezones
                            |> List.map resolvedTimezone
                            |> (List.uniqueBy (TimeZone.abbreviation (ZonedDateTime.toTimestamp viewModel.localtime)))
                            |> List.sortWith timezoneComparison
                in
                    div [ class "app" ]
                        [ div [ class "header" ]
                            [ div [ classList [ ( "title", True ), ( "connected", model.connected ) ] ]
                                [ div [ onClick (ToggleChannel model.connected) ] [ text "Who's Available?" ]
                                ]
                            , div [ class "time" ] [ text (formatTimeZone viewModel.localtime viewModel.userTimezone) ]
                            , div [ class "user" ]
                                [ div [] [ text viewModel.userName ]
                                , img [ src (gravatarUrl viewModel.userEmail), width 30, height 30 ] []
                                ]
                            ]
                        , div [ class "content" ]
                            [ div [ class "timezones" ]
                                (List.map
                                    (zoneColumn viewModel.localtime model.colleagues)
                                    offsets
                                )
                            ]
                        ]


zoneColumn : ZonedDateTime -> List Person -> TimeZone -> Html Msg
zoneColumn time colleagues zone =
    let
        people =
            colleagues
                |> List.filter (\colleague -> (TimeZone.abbreviation (ZonedDateTime.toTimestamp time) zone) == (TimeZone.abbreviation (ZonedDateTime.toTimestamp time) (resolvedTimezone (resolveTimezone colleague.timezone))))
    in
        div [ class "zone" ]
            ([ div [ class "timezone" ]
                [ text (formatTimeZone time zone)
                ]
             ]
                ++ (List.map (person time) people)
            )


person : ZonedDateTime -> Person -> Html Msg
person time person =
    let
        theirTimezone =
            (resolvedTimezone (resolveTimezone person.workingHours.timezone))

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
            (TimeZone.offset 0.0 zone1)

        offset2 =
            (TimeZone.offset 0.0 zone2)
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
        [ (toString (ZonedDateTime.hour localtime))
        , ":"
        , (String.padLeft 2 '0' (toString (ZonedDateTime.minute localtime)))
        , " "
        , (ZonedDateTime.abbreviation localtime)
        ]
            |> List.foldr (++) ""


modelToView : Model -> Maybe ViewModel
modelToView model =
    case model.user of
        Nothing ->
            Nothing

        Just user ->
            let
                timezones =
                    model.colleagues
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
    "https://www.gravatar.com/avatar/" ++ (gravatarHash email) ++ "?d=" ++ avatarType ++ "&s=" ++ (toString avatarSize)



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ (Time.every Time.second Tick), (Phoenix.Socket.listen model.phxSocket PhoenixMsg) ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = connect initModel
        , update = update
        , subscriptions = subscriptions
        }

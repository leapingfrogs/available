module App exposing (..)

import Html exposing (Html, text, div, span, img, br, button)
import Html.Attributes exposing (class, classList, src, width, height)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
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
import Json.Encode as JE
import Json.Decode as JD exposing (field)


---- MODEL ----


avatarSize : Int
avatarSize =
    100


avatarType : String
avatarType =
    "identicon"


type alias Model =
    { now : Maybe DateTime
    , user : Person
    , colleagues : List Person
    , phxSocket : Phoenix.Socket.Socket Msg
    , connected : Bool
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
    { start : ( Int, Int ), end : ( Int, Int ) }


initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init "ws://localhost:4000/socket/websocket"
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "data" "users:idavies@leapingfrogs.com" LoadData


init : ( Model, Cmd Msg )
init =
    let
        user =
            { name = "Ian Davies"
            , email = "idavies@leapingfrogs.com"
            , location = "Stirling, Scotland"
            , timezone = "Europe/London"
            , workingHours =
                { timezone = "Europe/London"
                , blocks =
                    [ { start = ( 9, 30 ), end = ( 14, 30 ) }
                    , { start = ( 15, 0 ), end = ( 18, 1 ) }
                    ]
                }
            , displayName = False
            }
    in
        ( { phxSocket = initPhxSocket
          , connected = False
          , now = Nothing
          , user = user
          , colleagues =
                [ user
                , { name = "One User"
                  , email = "one@flubber.com"
                  , location = "Paris, France"
                  , timezone = "Europe/Paris"
                  , workingHours =
                        { timezone =
                            "Europe/Paris"
                        , blocks =
                            [ { start = ( 19, 1 ), end = ( 20, 0 ) }
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
                            [ { start = ( 8, 0 ), end = ( 18, 0 ) }
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
                            [ { start = ( 7, 0 ), end = ( 20, 0 ) }
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
                            [ { start = ( 7, 0 ), end = ( 20, 0 ) }
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
                            [ { start = ( 9, 0 ), end = ( 18, 0 ) }
                            ]
                        }
                  , displayName = False
                  }
                ]
          }
        , Cmd.none
        )



---- UPDATE ----


type Msg
    = NoOp
    | ShowDetail Bool Person
    | Tick Time
    | LoadData JE.Value
    | ToggleChannel Bool
    | Joined
    | Closed
    | PhoenixMsg (Phoenix.Socket.Msg Msg)


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
            let
                debug =
                    Debug.log "Received: " raw
            in
                ( model, Cmd.none )

        ToggleChannel connected ->
            case connected of
                True ->
                    let
                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.leave "rooms:idavies@leapingfrogs.com" model.phxSocket
                    in
                        ( { model | phxSocket = phxSocket }
                        , Cmd.map PhoenixMsg phxCmd
                        )

                False ->
                    let
                        channel =
                            Phoenix.Channel.init "user:idavies@leapingfrogs.com"
                                |> Phoenix.Channel.withPayload (JE.object [])
                                |> Phoenix.Channel.onJoin (always Joined)
                                |> Phoenix.Channel.onClose (always Closed)

                        ( phxSocket, phxCmd ) =
                            Phoenix.Socket.join channel model.phxSocket
                    in
                        ( { model | phxSocket = phxSocket }, Cmd.map PhoenixMsg phxCmd )

        Joined ->
            let
                payload =
                    (JE.object [])

                push_ =
                    Phoenix.Push.init "load:data" "user:idavies@leapingfrogs.com"
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
            ( { model | connected = False }, Cmd.none )

        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )



---- VIEW ----


type alias ViewModel =
    { userName : String
    , userEmail : String
    , userTimezone : TimeZone
    , localtime : ZonedDateTime
    , timezones : List Resolved
    }


view : Model -> Html Msg
view model =
    let
        viewModel =
            modelToView model

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

                        ( startHour, startMinute ) =
                            startWork

                        ( endHour, endMinute ) =
                            endWork

                        ( nowHour, nowMinute ) =
                            nowTimeTuple

                        afterStart =
                            nowHour > startHour || (nowHour == startHour && nowMinute >= startMinute)

                        beforeEnd =
                            nowHour < endHour || (nowHour == endHour && nowMinute < endMinute)
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


modelToView : Model -> ViewModel
modelToView model =
    let
        timezones =
            model.colleagues
                |> List.map .timezone
                |> List.map resolveTimezone

        userTimezone =
            resolvedTimezone (resolveTimezone model.user.timezone)
    in
        { userName = model.user.name
        , userEmail = model.user.email
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
        , init = init
        , update = update
        , subscriptions = subscriptions
        }

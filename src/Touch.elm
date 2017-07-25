module Touch
    exposing
        ( Event
        , Gesture
        , blanco
        , deltaX
        , deltaY
        , isDownSwipe
        , isLeftSwipe
        , isRightSwipe
        , isTap
        , isUpSwipe
        , onEnd
        , onMove
        , onStart
        , record
        )

{-| Early stages of gesture recognition for touch-events.

This is intended to be used in qualified form.


# Hooking it up

In your model:

    { gesture : Touch.Gesture }

In your init:

    { gesture = Touch.blanco }

In your Msg:

    type Msg
        = Swipe Touch.Event
        | SwipeEnd Touch.Event

In your view:

    Html.div
        [ Touch.onStart Swipe
        , Touch.onMove Swipe
        , Touch.onEnd SwipeEnd
        ]
        [ Html.text "Swipe me!" ]

In your update:

    Swipe touch ->
        { model | gesture = Touch.record touch model.gesture }

    SwipeEnd touch ->
        let
            gesture : Touch.Gesture
            gesture =
                Touch.record touch model.gesture

            -- use inspection functions like `isTap` and `isLeftSwipe`
        in
        { model | gesture = Touch.blanco }


# Events stuff

@docs onMove, onEnd, onStart


# Keep some state around

@docs Gesture, Event, blanco, record


# Get yourself some info

@docs deltaX, deltaY, isTap, isUpSwipe, isDownSwipe, isLeftSwipe, isRightSwipe

-}

import Html
import Html.Events exposing (on, onWithOptions)
import Json.Decode as Json exposing (Decoder)


{-| Checks if a given gesture is actually a (single) tap
-}
isTap : Gesture -> Bool
isTap gesture =
    case gesture of
        EndTap _ ->
            True

        _ ->
            False


{-| For a finished move, checks how much you move horizontally, from start to
finish.
-}
deltaX : Gesture -> Maybe Float
deltaX gesture =
    case gesture of
        EndGesture { from, to } ->
            Just (to.x - from.x)

        _ ->
            Nothing


{-| For a finished move, checks how much you move vertically, from start to
finish.
-}
deltaY : Gesture -> Maybe Float
deltaY gesture =
    case gesture of
        EndGesture { from, to } ->
            Just (to.y - from.y)

        _ ->
            Nothing


{-| Is this gesture finished and did we move more than `sensitivity` to the right?
-}
isRightSwipe : Float -> Gesture -> Bool
isRightSwipe sensitivity =
    isSwipeType deltaX (\dX -> dX >= sensitivity)


{-| Is this gesture finished and did we move more than `sensitivity` to the left?
-}
isLeftSwipe : Float -> Gesture -> Bool
isLeftSwipe sensitivity =
    isSwipeType deltaX (\dX -> dX <= -sensitivity)


{-| Is this gesture finished and did we move more than `sensitivity` to the bottom?
-}
isDownSwipe : Float -> Gesture -> Bool
isDownSwipe sensitivity =
    isSwipeType deltaY (\dY -> dY >= sensitivity)


{-| Is this gesture finished and did we move more than `sensitivity` to the top?
-}
isUpSwipe : Float -> Gesture -> Bool
isUpSwipe sensitivity =
    isSwipeType deltaY (\dY -> dY <= -sensitivity)


isSwipeType : (Gesture -> Maybe Float) -> (Float -> Bool) -> Gesture -> Bool
isSwipeType delta predicate =
    delta >> Maybe.map predicate >> Maybe.withDefault False


type alias Coordinate =
    { x : Float, y : Float }


type alias Trail =
    { from : Coordinate, through : List Coordinate, to : Coordinate }


{-| A `Gesture`! You'll want to keep one of these around in your model and
update it whenever applicable.
-}
type Gesture
    = None
    | Started Coordinate
    | Moved Trail
    | EndGesture Trail
    | EndTap Coordinate


{-| A single `Touch.Event`. Gestures are made up of these, internally.
-}
type Event
    = Touch EventType Coordinate


type EventType
    = Start
    | Move
    | End


{-| Get yourself a blanco gesture, as if no touches have happened at all.

After a touchend event, you'll probably want to reset to this, too.

-}
blanco : Gesture
blanco =
    None


addToTrail : Coordinate -> Trail -> Trail
addToTrail coordinate { from, to, through } =
    { from = from, through = to :: through, to = coordinate }


{-| Our cute little `update`-like function!
-}
record : Event -> Gesture -> Gesture
record (Touch eventType coordinate) gesture =
    case ( eventType, gesture ) of
        ( Start, _ ) ->
            Started coordinate

        ( Move, Started prev ) ->
            Moved { from = prev, through = [], to = coordinate }

        ( Move, Moved trail ) ->
            addToTrail coordinate trail |> Moved

        ( Move, _ ) ->
            Started coordinate

        ( End, Moved trail ) ->
            addToTrail coordinate trail |> EndGesture

        ( End, _ ) ->
            EndTap coordinate


decodeTouch : String -> (Coordinate -> msg) -> Decoder msg
decodeTouch fieldName tagger =
    Json.map2 Coordinate
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)
        |> Json.at [ fieldName, "0" ]
        |> Json.map tagger


{-| Record the start of a touch gesture.
-}
onStart : (Event -> msg) -> Html.Attribute msg
onStart tagger =
    on "touchstart" <| decodeTouch "touches" (Touch Start >> tagger)


{-| Record an ongoing touch gesture.
-}
onMove : (Event -> msg) -> Html.Attribute msg
onMove tagger =
    on "touchmove" <| decodeTouch "changedTouches" (Touch Move >> tagger)


{-| Record the end of a touch gesture.
-}
onEnd : (Event -> msg) -> Html.Attribute msg
onEnd tagger =
    onWithOptions "touchend"
        { stopPropagation = False
        , preventDefault = True
        }
    <|
        decodeTouch "changedTouches" (Touch End >> tagger)

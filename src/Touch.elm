module Touch
    exposing
        ( Gesture
        , Touch
        , blanco
        , deltaX
        , deltaY
        , isTap
        , onEnd
        , onMove
        , onStart
        , record
        )

{-| Early stages of gesture recognition for touch-events.


# Hooking it up

@docs onMove, onEnd, onStart, record


# For keeping around

@docs Gesture, Touch, blanco


# Getting some info

@docs deltaX, deltaY, isTap

-}

import Html
import Html.Events exposing (on)
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
            Just (from.x - to.x)

        _ ->
            Nothing


{-| For a finished move, checks how much you move vertically, from start to
finish.
-}
deltaY : Gesture -> Maybe Float
deltaY gesture =
    case gesture of
        EndGesture { from, to } ->
            Just (from.y - to.y)

        _ ->
            Nothing


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


{-| A single `Touch` event. Gestures are made up of these, internally.
-}
type Touch
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
record : Touch -> Gesture -> Gesture
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
onStart : (Touch -> msg) -> Html.Attribute msg
onStart tagger =
    on "touchstart" <| decodeTouch "touches" (Touch Start >> tagger)


{-| Record an ongoing touch gesture.
-}
onMove : (Touch -> msg) -> Html.Attribute msg
onMove tagger =
    on "touchmove" <| decodeTouch "changedTouches" (Touch Move >> tagger)


{-| Record the end of a touch gesture.
-}
onEnd : (Touch -> msg) -> Html.Attribute msg
onEnd tagger =
    on "touchend" <| decodeTouch "changedTouches" (Touch End >> tagger)

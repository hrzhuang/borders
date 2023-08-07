module Animate exposing (Animated, Config, Wrap(..), with, get, isActive, to,
    frame)

type Wrap = NoWrap | Wrap Float Float

type Animated = Animated
    -- spring config
    { mass : Float
    , stiffness : Float
    , damping : Float
    , wrap : Wrap
    , displacementTolerance : Float
    , velocityTolerance : Float
    -- animation state
    , value : Float
    , target : Float
    , velocity : Float
    , active : Bool
    }

type alias Config =
    { mass : Float
    , stiffness : Float
    , damping : Float
    , wrap : Wrap
    , displacementTolerance : Float
    , velocityTolerance : Float
    }

with : Config -> Float -> Animated
with { mass, stiffness, damping, wrap, displacementTolerance,
        velocityTolerance } init =
    Animated
        { mass = mass
        , stiffness = stiffness
        , damping = damping
        , wrap = wrap
        , displacementTolerance = displacementTolerance
        , velocityTolerance = velocityTolerance
        , value = init
        , target = init
        , velocity = 0
        , active = False
        }

get : Animated -> Float
get (Animated { value }) = value

isActive : Animated -> Bool
isActive (Animated { active }) = active

to : Float -> Animated -> Animated
to target (Animated animated) =
    Animated { animated | target = target, active = True }

wrapDisplacement : Wrap -> Float -> Float -> Float
wrapDisplacement wrap value target =
    let
        noWrapDisplacement = value - target
    in case wrap of
        NoWrap -> noWrapDisplacement
        Wrap start end ->
            let
                wrapLeftDisplacement = value - (start - (end - target))
                wrapRightDisplacement = value - (end + (target - start))
            in if abs wrapLeftDisplacement < abs noWrapDisplacement then
                wrapLeftDisplacement
            else if abs wrapRightDisplacement < abs noWrapDisplacement then
                wrapRightDisplacement
            else
                noWrapDisplacement

wrapValue : Wrap -> Float -> Float
wrapValue wrap value = case wrap of
    NoWrap ->
        value
    Wrap start end ->
        let
            period = end - start
        in if value > end then
            value - (toFloat << floor) ((value - start) / period) * period
        else if value < start then
            value + (toFloat << ceiling) ((start - value) / period) * period
        else
            value

frame : Float -> Animated -> Animated
frame delta (Animated animated) =
    let
        { mass, stiffness, damping, wrap, displacementTolerance,
            velocityTolerance, value, target, velocity, active } = animated
        seconds = delta / 1000
        displacement = wrapDisplacement wrap value target
        acceleration = (negate stiffness * displacement - damping * velocity)
            / mass
    in if not active then
        Animated animated
    else if abs displacement < displacementTolerance
            && abs velocity < velocityTolerance then
        Animated
            { animated
            | value = target
            , velocity = 0
            , active = False
            }
    else Animated
        { animated
        | value = wrapValue wrap (value + seconds * velocity)
        , velocity = velocity + seconds * acceleration
        }

port module Main exposing (main)

import Task

import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Random

import Animate exposing (Animated)
import Countries exposing (Countries, Country, GeoCoords)

type alias Flags = ()

type alias GeneratingInitialCountryModel =
    { windowWidth : Maybe Int
    , windowHeight : Maybe Int
    , pageVisibility : Browser.Events.Visibility
    }

type alias FinishingInitializationModel =
    { windowWidth : Maybe Int
    , windowHeight : Maybe Int
    , pageVisibility : Browser.Events.Visibility
    , countries : Countries
    , country : Country
    , jsInitialized : Bool
    }

type alias InitializedModel a =
    { a
    | windowWidth : Int
    , windowHeight : Int
    , pageVisibility : Browser.Events.Visibility
    , countries : Countries
    , countryName : String
    , answer : String
    , cameraDistance : Animated
    , cameraLatitude : Animated
    , cameraLongitude : Animated
    }

type alias AnsweringModel = InitializedModel
    { countryAlternativeNames : List String }

type alias SubmittedModel a = InitializedModel { a | correct : Bool }

type alias DisplayResultsModel = SubmittedModel {}

type alias GeneratingNewCountryModel = SubmittedModel
    { continuedUsingEnterKey : Bool }

type alias WaitingForJsLoadNewCountryModel = SubmittedModel
    { continuedUsingEnterKey : Bool
    , newCountry : Country
    }

type Model
    = GeneratingInitialCountryState GeneratingInitialCountryModel
    | FinishingInitializationState FinishingInitializationModel
    | AnsweringState AnsweringModel
    | DisplayResultsState DisplayResultsModel
    | GeneratingNewCountryState GeneratingNewCountryModel
    | WaitingForJsLoadNewCountryState WaitingForJsLoadNewCountryModel
    | ErrorState

type Msg
    = GotCountries (Country, Countries)
    | GenerateCountryError -- this should never actually happen
    | JsReady
    | AnswerUpdated String
    | EnterKeyPressed
    | NextCountryButtonClicked
    | AnswerFieldFocused
    | AnswerFieldFocusError -- this should never actually happen
    | GotWindowDimensions Int Int
    | PageVisibilityChange Browser.Events.Visibility
    | AnimationFrame Float

main : Program Flags Model Msg
main = Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

{- ---
 - ports
 - ---
 -}

type alias Dot =
    { latitude : Float
    , longitude : Float
    , radius : Float
    }

type alias WindowDimensions = { width : Int, height : Int }

port sendMesh : Encode.Value -> Cmd msg
port fillCountryByCode : String -> Cmd msg
port drawDots : List Dot -> Cmd msg
port sendWindowDimensionsInitializing : WindowDimensions -> Cmd msg
port sendWindowDimensionsInitialized : WindowDimensions -> Cmd msg
port sendUniforms : Encode.Value -> Cmd msg
port sendCorrect : () -> Cmd msg
port sendWrong : () -> Cmd msg
port jsReadySignal : (() -> msg) -> Sub msg

fillCountry : Country -> Cmd msg
fillCountry { code } = fillCountryByCode code

tinyDotRadius : Float
tinyDotRadius = 0.15

smallDotRadius : Float
smallDotRadius = 0.25

mediumDotRadius : Float
mediumDotRadius = 0.5

largeDotRadius : Float
largeDotRadius = 1

scaleDotRadius : Countries.Scale -> Float
scaleDotRadius scale = case scale of
    Countries.Small -> smallDotRadius
    Countries.Medium -> mediumDotRadius
    Countries.Large -> largeDotRadius

scaleSmallDotRadius : Countries.Scale -> Float
scaleSmallDotRadius scale = case scale of
    Countries.Small -> tinyDotRadius
    Countries.Medium -> smallDotRadius
    Countries.Large -> mediumDotRadius

countryDot : Country -> Dot
countryDot { latitude, longitude, scale } =
    { latitude = latitude
    , longitude = longitude
    , radius = scaleDotRadius scale
    }

smallDot : Countries.Scale -> GeoCoords -> Dot
smallDot scale { latitude, longitude } =
    { latitude = latitude
    , longitude = longitude
    , radius = scaleSmallDotRadius scale
    }

highlightCountry : Country -> Cmd msg
highlightCountry country = case country.highlightMethod of
    Countries.Fill -> fillCountry country
    Countries.Dot -> countryDot country |> List.singleton |> drawDots
    Countries.SmallDots coords -> List.map (smallDot country.scale) coords
        |> drawDots

{- ---
 - misc utilities
 - ---
 -}

generateCountryResult : (Maybe Country, Countries) -> Msg
generateCountryResult result = case result of
    (Just country, countries) -> GotCountries (country, countries)
    (Nothing, _) -> GenerateCountryError

countryCameraDistance : Country -> Float
countryCameraDistance country = case country.scale of
    Countries.Small -> 0.75
    Countries.Medium -> 1.25
    Countries.Large -> 2.5

enterKeyDecoder : Decoder Msg
enterKeyDecoder =
    let
        check key = case key == "Enter" of
            True -> Decode.succeed EnterKeyPressed
            False -> Decode.fail "Not enter key"
    in
    Decode.field "key" Decode.string
        |> Decode.andThen check

{- ---
 - init
 - ---
 -}

gotViewport : Dom.Viewport -> Msg
gotViewport { viewport } =
    GotWindowDimensions (round viewport.width) (round viewport.height)

getWindowDimensions : Cmd Msg
getWindowDimensions = Task.perform gotViewport Dom.getViewport

init : Flags -> (Model, Cmd Msg)
init () =
    ( GeneratingInitialCountryState
        { windowWidth = Nothing
        , windowHeight = Nothing
        -- this is a guess, but no significant harm when we are wrong
        , pageVisibility = Browser.Events.Visible
        }
    , Cmd.batch
        [ getWindowDimensions
        , sendMesh meshJson
        , Random.generate generateCountryResult (Countries.next Countries.init)
        ]
    )

{- ---
 - update
 - ---
 -}

animationConfig : Animate.Wrap -> Animate.Config
animationConfig wrap =
    { mass = 1
    , stiffness = 5
    , damping = 4
    , wrap = wrap
    , displacementTolerance = 1e-3
    , velocityTolerance = 1e-3
    }

render : InitializedModel a -> Cmd msg
render model = uniforms model |> encodeUniforms |> sendUniforms

tryFinishInitialization : FinishingInitializationModel -> (Model, Cmd Msg)
tryFinishInitialization finishingModel =
    case (finishingModel.windowWidth, finishingModel.windowHeight,
            finishingModel.jsInitialized) of
        (Just windowWidth, Just windowHeight, True) ->
            let
                { pageVisibility, countries, country } = finishingModel
                answeringModel =
                    { windowWidth = windowWidth
                    , windowHeight = windowHeight
                    , pageVisibility = pageVisibility
                    , countries = countries
                    , countryName = country.name
                    , cameraDistance = Animate.with
                        (animationConfig Animate.noWrap)
                        (countryCameraDistance country)
                    , cameraLatitude = Animate.with
                        (animationConfig Animate.noWrap)
                        country.latitude
                    , cameraLongitude = Animate.with
                        (animationConfig <| Animate.wrap -180 180)
                        country.longitude
                    , countryAlternativeNames = country.alternativeNames
                    , answer = ""
                    }
            in
            ( AnsweringState answeringModel
            , render answeringModel
            )
        _ -> ( FinishingInitializationState finishingModel, Cmd.none )

checkAnswer : AnsweringModel -> Bool
checkAnswer { countryName, countryAlternativeNames, answer } =
    let
        lowerName = String.toLower countryName
        lowerAlternativeNames = List.map String.toLower countryAlternativeNames
        lowerAnswer = String.toLower answer
    in
    lowerAnswer == lowerName || List.member lowerAnswer lowerAlternativeNames

answerFieldFocusResult : Result Dom.Error () -> Msg
answerFieldFocusResult result = case result of
    Ok _ -> AnswerFieldFocused
    Err _ -> AnswerFieldFocusError

updateAnimation : Float -> InitializedModel a -> InitializedModel a
updateAnimation delta model =
    let
        { cameraDistance, cameraLatitude, cameraLongitude } = model
    in
        { model
        | cameraDistance = Animate.frame delta cameraDistance
        , cameraLatitude = Animate.frame delta cameraLatitude
        , cameraLongitude = Animate.frame delta cameraLongitude
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case model of
    GeneratingInitialCountryState generatingModel -> case msg of
        GotCountries (country, countries) ->
            let
                { windowWidth, windowHeight, pageVisibility } = generatingModel
            in
            ( FinishingInitializationState
                { windowWidth = windowWidth
                , windowHeight = windowHeight
                , pageVisibility = pageVisibility
                , countries = countries
                , country = country
                , jsInitialized = False
                }
            , highlightCountry country
            )
        GotWindowDimensions width height ->
            ( GeneratingInitialCountryState
                { generatingModel
                | windowWidth = Just width
                , windowHeight = Just height
                }
            , sendWindowDimensionsInitializing
                { width = width, height = height }
            )
        PageVisibilityChange visibility ->
            ( GeneratingInitialCountryState
                { generatingModel | pageVisibility = visibility }
            , Cmd.none
            )
        _ -> (ErrorState, Cmd.none)
    FinishingInitializationState finishingModel -> case msg of
        JsReady ->
            tryFinishInitialization { finishingModel | jsInitialized = True }
        GotWindowDimensions width height ->
            let
                (newModel, cmd) = tryFinishInitialization
                    { finishingModel
                    | windowWidth = Just width
                    , windowHeight = Just height
                    }
            in
            ( newModel
            , Cmd.batch
                [ cmd
                , sendWindowDimensionsInitializing
                    { width = width, height = height }
                ]
            )
        PageVisibilityChange visibility ->
            ( FinishingInitializationState
                { finishingModel | pageVisibility = visibility }
            , Cmd.none
            )
        _ -> (ErrorState, Cmd.none)
    AnsweringState answeringModel -> case msg of
        AnswerUpdated newAnswer ->
            ( AnsweringState { answeringModel | answer = newAnswer }
            , Cmd.none
            )
        EnterKeyPressed ->
            let
                { windowWidth, windowHeight, pageVisibility, countries,
                    countryName, answer, cameraDistance, cameraLatitude,
                    cameraLongitude } = answeringModel
                correct = checkAnswer answeringModel
            in
            ( DisplayResultsState
                { windowWidth = windowWidth
                , windowHeight = windowHeight
                , pageVisibility = pageVisibility
                , countries = countries
                , countryName = countryName
                , answer = answer
                , cameraDistance = cameraDistance
                , cameraLatitude = cameraLatitude
                , cameraLongitude = cameraLongitude
                , correct = correct
                }
            , case correct of
                True -> sendCorrect ()
                False -> sendWrong ()
            )
        AnswerFieldFocused -> ( AnsweringState answeringModel, Cmd.none )
        GotWindowDimensions width height ->
            let
                newAnsweringModel =
                    { answeringModel
                    | windowWidth = width
                    , windowHeight = height
                    }
            in
            ( AnsweringState newAnsweringModel
            , Cmd.batch
                [ sendWindowDimensionsInitialized
                    { width = width, height = height }
                , render newAnsweringModel
                ]
            )
        PageVisibilityChange visibility ->
            ( AnsweringState { answeringModel | pageVisibility = visibility }
            , Cmd.none
            )
        AnimationFrame delta ->
            let
                newAnsweringModel = updateAnimation delta answeringModel
            in
            ( AnsweringState newAnsweringModel
            , render newAnsweringModel
            )
        _ -> (ErrorState, Cmd.none)
    DisplayResultsState resultsModel -> case msg of
        EnterKeyPressed ->
            let
                { windowWidth, windowHeight, pageVisibility, countries,
                    countryName, answer, cameraDistance, cameraLatitude,
                    cameraLongitude, correct } = resultsModel
            in
            ( GeneratingNewCountryState
                { windowWidth = windowWidth
                , windowHeight = windowHeight
                , pageVisibility = pageVisibility
                , countries = countries
                , countryName = countryName
                , answer = answer
                , cameraDistance = cameraDistance
                , cameraLatitude = cameraLatitude
                , cameraLongitude = cameraLongitude
                , correct = correct
                , continuedUsingEnterKey = True
                }
            , Random.generate generateCountryResult (Countries.next countries)
            )
        NextCountryButtonClicked ->
            let
                { windowWidth, windowHeight, pageVisibility, countries,
                    countryName, answer, cameraDistance, cameraLatitude,
                    cameraLongitude, correct } = resultsModel
            in
            ( GeneratingNewCountryState
                { windowWidth = windowWidth
                , windowHeight = windowHeight
                , pageVisibility = pageVisibility
                , countries = countries
                , countryName = countryName
                , answer = answer
                , cameraDistance = cameraDistance
                , cameraLatitude = cameraLatitude
                , cameraLongitude = cameraLongitude
                , correct = correct
                , continuedUsingEnterKey = False
                }
            , Random.generate generateCountryResult (Countries.next countries)
            )
        AnswerFieldFocused -> ( DisplayResultsState resultsModel, Cmd.none )
        GotWindowDimensions width height ->
            let
                newResultsModel =
                    { resultsModel
                    | windowWidth = width
                    , windowHeight = height
                    }
            in
            ( DisplayResultsState newResultsModel
            , Cmd.batch
                [ sendWindowDimensionsInitialized
                    { width = width, height = height }
                , render newResultsModel
                ]
            )
        PageVisibilityChange visibility ->
            ( DisplayResultsState
                { resultsModel | pageVisibility = visibility }
            , Cmd.none
            )
        AnimationFrame delta ->
            let
                newResultsModel = updateAnimation delta resultsModel
            in
            ( DisplayResultsState newResultsModel
            , render newResultsModel
            )
        _ -> (ErrorState, Cmd.none)
    GeneratingNewCountryState generatingModel -> case msg of
        GotCountries (country, countries) ->
            let
                { windowWidth, windowHeight, pageVisibility, countryName,
                    answer, cameraDistance, cameraLatitude, cameraLongitude,
                    correct, continuedUsingEnterKey } = generatingModel
            in
            ( WaitingForJsLoadNewCountryState
                { windowWidth = windowWidth
                , windowHeight = windowHeight
                , pageVisibility = pageVisibility
                , countries = countries
                , countryName = countryName
                , answer = answer
                , cameraDistance = cameraDistance
                , cameraLatitude = cameraLatitude
                , cameraLongitude = cameraLongitude
                , correct = correct
                , continuedUsingEnterKey = continuedUsingEnterKey
                , newCountry = country
                }
            , highlightCountry country
            )
        AnswerFieldFocused ->
            ( GeneratingNewCountryState generatingModel, Cmd.none )
        GotWindowDimensions width height ->
            let
                newGeneratingModel =
                    { generatingModel
                    | windowWidth = width
                    , windowHeight = height
                    }
            in
            ( GeneratingNewCountryState newGeneratingModel
            , Cmd.batch
                [ sendWindowDimensionsInitialized
                    { width = width, height = height }
                , render newGeneratingModel
                ]
            )
        PageVisibilityChange visibility ->
            ( GeneratingNewCountryState
                { generatingModel | pageVisibility = visibility }
            , Cmd.none
            )
        AnimationFrame delta ->
            let
                newGeneratingModel = updateAnimation delta generatingModel
            in
            ( GeneratingNewCountryState newGeneratingModel
            , render newGeneratingModel
            )
        _ -> (ErrorState, Cmd.none)
    WaitingForJsLoadNewCountryState waitingModel -> case msg of
        JsReady ->
            let
                { windowWidth, windowHeight, pageVisibility, countries,
                    answer, cameraDistance, cameraLatitude, cameraLongitude,
                    continuedUsingEnterKey, newCountry } = waitingModel
                answeringModel =
                    { windowWidth = windowWidth
                    , windowHeight = windowHeight
                    , pageVisibility = pageVisibility
                    , countries = countries
                    , countryName = newCountry.name
                    , answer = ""
                    , cameraDistance = Animate.to
                        (countryCameraDistance newCountry)
                        cameraDistance
                    , cameraLatitude =
                        Animate.to newCountry.latitude cameraLatitude
                    , cameraLongitude =
                        Animate.to newCountry.longitude cameraLongitude
                    , countryAlternativeNames = newCountry.alternativeNames
                    }
            in
            ( AnsweringState answeringModel
            , Cmd.batch
                [ render answeringModel
                , case continuedUsingEnterKey of
                    True -> Dom.focus "answer"
                        |> Task.attempt answerFieldFocusResult
                    False -> Cmd.none
                ]
            )
        AnswerFieldFocused ->
            ( WaitingForJsLoadNewCountryState waitingModel, Cmd.none )
        GotWindowDimensions width height ->
            let
                newWaitingModel =
                    { waitingModel
                    | windowWidth = width
                    , windowHeight = height
                    }
            in
            ( WaitingForJsLoadNewCountryState newWaitingModel
            , Cmd.batch
                [ sendWindowDimensionsInitialized
                    { width = width, height = height }
                , render newWaitingModel
                ]
            )
        PageVisibilityChange visibility ->
            ( WaitingForJsLoadNewCountryState
                { waitingModel | pageVisibility = visibility }
            , Cmd.none
            )
        AnimationFrame delta ->
            let
                newWaitingModel = updateAnimation delta waitingModel
            in
            ( WaitingForJsLoadNewCountryState newWaitingModel
            , render newWaitingModel
            )
        _ -> (ErrorState, Cmd.none)
    ErrorState -> (ErrorState, Cmd.none)

{- ---
 - subscriptions
 - ---
 -}

jsReady : () -> Msg
jsReady () = JsReady

animationSubscriptions : InitializedModel a -> Sub Msg
animationSubscriptions { pageVisibility, cameraDistance, cameraLatitude,
        cameraLongitude } =
    if pageVisibility == Browser.Events.Visible
            && (Animate.isActive cameraDistance
            || Animate.isActive cameraLatitude
            || Animate.isActive cameraLongitude) then
        Browser.Events.onAnimationFrameDelta AnimationFrame
    else
        Sub.none

subscriptions : Model -> Sub Msg
subscriptions model = case model of
    GeneratingInitialCountryState _ -> Sub.batch
        [ Browser.Events.onResize GotWindowDimensions
        , Browser.Events.onVisibilityChange PageVisibilityChange
        ]
    FinishingInitializationState _ -> Sub.batch
        [ Browser.Events.onResize GotWindowDimensions
        , Browser.Events.onVisibilityChange PageVisibilityChange
        , jsReadySignal jsReady
        ]
    AnsweringState answeringModel -> Sub.batch
        [ Browser.Events.onResize GotWindowDimensions
        , Browser.Events.onVisibilityChange PageVisibilityChange
        , animationSubscriptions answeringModel
        ]
    DisplayResultsState resultsModel -> Sub.batch
        [ Browser.Events.onResize GotWindowDimensions
        , Browser.Events.onVisibilityChange PageVisibilityChange
        , Browser.Events.onKeyUp enterKeyDecoder
        , animationSubscriptions resultsModel
        ]
    GeneratingNewCountryState generatingModel -> Sub.batch
        [ Browser.Events.onResize GotWindowDimensions
        , Browser.Events.onVisibilityChange PageVisibilityChange
        , animationSubscriptions generatingModel
        ]
    WaitingForJsLoadNewCountryState waitingModel -> Sub.batch
        [ Browser.Events.onResize GotWindowDimensions
        , Browser.Events.onVisibilityChange PageVisibilityChange
        , jsReadySignal jsReady
        , animationSubscriptions waitingModel
        ]
    ErrorState -> Sub.none

{- ---
 - view
 - ---
 -}

viewUiContainer : List (Html msg) -> Html msg
viewUiContainer = Html.div [ Attrs.id "ui-container" ]

viewAnswering : AnsweringModel -> List (Html Msg)
viewAnswering { answer } =
    let
        decoder = Decode.map (\msg -> (msg, True)) enterKeyDecoder
    in
    [ Html.div [ Attrs.class "heading" ] [ Html.text "What country is this?" ]
    , Html.input
        [ Attrs.id "answer"
        , Attrs.placeholder "Answer"
        , Attrs.value answer
        , Html.Events.onInput AnswerUpdated
        , Html.Events.stopPropagationOn "keyup" decoder
        ]
        []
    ]

viewSubmitted : Bool -> SubmittedModel a -> List (Html Msg)
viewSubmitted nextCountryButtonEnabled { countryName, answer, correct } =
    let
        heading = case correct of
            True -> "Correct!"
            False -> "Incorrect..."
        incorrect = case correct of
            True -> []
            False ->
                [ Html.div
                    [ Attrs.id "incorrect" ]
                    [ Html.text ("❌ " ++ answer) ]
                ]
    in
    [ Html.div [ Attrs.class "heading" ] [ Html.text heading ]
    , Html.div [ Attrs.id "incorrect-correct" ] <|
        incorrect ++
        [ Html.div
            [ Attrs.id "correct" ]
            [ Html.text ("✅ " ++ countryName) ]
        ]
    , Html.button
        [ Attrs.id "continue"
        , Attrs.disabled (not nextCountryButtonEnabled)
        , Html.Events.onClick NextCountryButtonClicked
        ]
        [ Html.text "Continue" ]
    ]

view : Model -> Html Msg
view model = case model of
    GeneratingInitialCountryState _ -> Html.div [] []
    FinishingInitializationState _ -> Html.div [] []
    AnsweringState answeringModel ->
        viewUiContainer (viewAnswering answeringModel)
    DisplayResultsState resultsModel ->
        viewUiContainer (viewSubmitted True resultsModel)
    GeneratingNewCountryState generatingModel ->
        viewUiContainer (viewSubmitted False generatingModel)
    WaitingForJsLoadNewCountryState waitingModel ->
        viewUiContainer (viewSubmitted False waitingModel)
    ErrorState -> Html.div [] []

{- ---
 - mesh
 - ---
 -}

type alias Vertex =
    { pos : Vec3
    , textureCoords : Vec2
    }

type alias Triangle = (Vertex, Vertex, Vertex)

type alias Quad = { tl : Vertex, tr : Vertex, bl : Vertex, br : Vertex }

numUSlices : Int
numUSlices = numVSlices * 2

numVSlices : Int
numVSlices = 64

uvVertex : Float -> Float -> Vertex
uvVertex u v =
    let
        theta = v * pi
        phi = u * 2*pi
    in
    { pos = vec3 (sin theta * cos phi) (sin theta * sin phi) (cos theta)
    , textureCoords = vec2 u v
    }

type alias UInterval =
    { start : Float
    , end : Float
    , mid : Float
    }

ithUInterval : Int -> UInterval
ithUInterval i =
    let
        start = toFloat i / toFloat numUSlices
        end = toFloat (i+1) / toFloat numUSlices
    in
    { start = start
    , end = end
    , mid = (start + end) / 2
    }

uIntervals : List UInterval
uIntervals = List.range 0 (numUSlices - 1)
    |> List.map ithUInterval

polarTriangle : Float -> Float -> UInterval -> Triangle
polarTriangle vPole vCircle uInt =
    ( uvVertex uInt.mid vPole
    , uvVertex uInt.start vCircle
    , uvVertex uInt.end vCircle
    )

northPoleV : Float
northPoleV = 0

southPoleV : Float
southPoleV = 1

northCircleV : Float
northCircleV = 1 / toFloat numVSlices

southCircleV : Float
southCircleV = toFloat (numVSlices - 1) / toFloat numVSlices

northTriangles : List Triangle
northTriangles = List.map (polarTriangle northPoleV northCircleV) uIntervals

southTriangles : List Triangle
southTriangles = List.map (polarTriangle southPoleV southCircleV) uIntervals

polarTriangles : List Triangle
polarTriangles = northTriangles ++ southTriangles

type alias VInterval = (Float, Float)

ithVInterval : Int -> VInterval
ithVInterval i =
    let
        start = toFloat i / toFloat numVSlices
        end = toFloat (i+1) / toFloat numVSlices
    in (start, end)

uvQuad : VInterval -> UInterval -> Quad
uvQuad (vStart, vEnd) uInt =
    { tl = uvVertex uInt.start vStart
    , tr = uvVertex uInt.end vStart
    , bl = uvVertex uInt.start vEnd
    , br = uvVertex uInt.end vEnd
    }

vSliceQuads : VInterval -> List Quad
vSliceQuads vInt = List.map (uvQuad vInt) uIntervals

quads : List Quad
quads = List.range 1 (numVSlices - 2)
    |> List.map ithVInterval
    |> List.concatMap vSliceQuads

quadTriangles : Quad -> List Triangle
quadTriangles { tl, tr, bl, br } = [(tl,tr,bl), (tr,bl,br)]

mesh : List Triangle
mesh = polarTriangles ++ List.concatMap quadTriangles quads

{- ---
 - uniforms
 - ---
 -}

type alias Uniforms =
    { rotation : Mat4
    , camera : Mat4
    , perspective : Mat4
    , ambientBrightness : Float
    , diffuseBrightness : Float
    , specularBrightness : Float
    , lightColor : Vec3
    , lightDir : Vec3
    , cameraPos : Vec3
    , shininess : Float
    }

white : Vec3
white = vec3 1 1 1

uniforms : InitializedModel a -> Uniforms
uniforms { windowWidth, windowHeight, cameraDistance, cameraLatitude,
        cameraLongitude } =
    let
        cameraPos = Vec3.scale
            (1 + Animate.get cameraDistance)
            (Vec3.negate Vec3.i)
        aspectRatio = toFloat windowWidth / toFloat windowHeight
    in
    { rotation =
        Mat4.makeRotate
            (Animate.get cameraLatitude |> degrees |> negate)
            Vec3.j
        |> Mat4.rotate
            (Animate.get cameraLongitude |> degrees |> negate)
            Vec3.k
    , camera = Mat4.makeLookAt cameraPos (vec3 0 0 0) Vec3.k
    , perspective = Mat4.makePerspective 45 aspectRatio 0.01 100
    , ambientBrightness = 0.5
    , diffuseBrightness = 0.5
    , specularBrightness = 0.1
    , lightColor = white
    , lightDir = Vec3.normalize <| vec3 -2 -1 1
    , cameraPos = cameraPos
    , shininess = 2
    }

{- ---
 - encoding
 - ---
 -}

vec2Components : Vec2 -> List Float
vec2Components v = [ Vec2.getX v, Vec2.getY v ]

vec3Components : Vec3 -> List Float
vec3Components v = [ Vec3.getX v, Vec3.getY v, Vec3.getZ v ]

triangleVertices : Triangle -> List Vertex
triangleVertices (v1,v2,v3) = [v1,v2,v3]

meshJson : Encode.Value
meshJson =
    let
        vertices = List.concatMap triangleVertices mesh
        posArray = List.concatMap (.pos >> vec3Components) vertices
        textureCoordsArray = vertices
            |> List.concatMap (.textureCoords >> vec2Components)
    in Encode.object
        [ ("pos", Encode.list Encode.float posArray)
        , ("textureCoords", Encode.list Encode.float textureCoordsArray)
        , ("numVertices", List.length vertices |> Encode.int)
        ]

encodeMat4 : Mat4 -> Encode.Value
encodeMat4 m =
    let
        { m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43,
            m14, m24, m34, m44 } = Mat4.toRecord m
    in Encode.list Encode.float
        -- column-major order used by webgl
        [m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43,
            m14, m24, m34, m44]

encodeVec3 : Vec3 -> Encode.Value
encodeVec3 = vec3Components >> Encode.list Encode.float

encodeUniforms : Uniforms -> Encode.Value
encodeUniforms { rotation, camera, perspective, ambientBrightness,
        diffuseBrightness, specularBrightness, lightColor, lightDir,
        cameraPos, shininess } =
    Encode.object
        [ ( "rotation", encodeMat4 rotation )
        , ( "camera", encodeMat4 camera )
        , ( "perspective", encodeMat4 perspective )
        , ( "ambientBrightness", Encode.float ambientBrightness )
        , ( "diffuseBrightness", Encode.float diffuseBrightness )
        , ( "specularBrightness", Encode.float specularBrightness )
        , ( "lightColor", encodeVec3 lightColor )
        , ( "lightDir", encodeVec3 lightDir )
        , ( "cameraPos", encodeVec3 cameraPos )
        , ( "shininess", Encode.float shininess )
        ]

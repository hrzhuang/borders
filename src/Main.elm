port module Main exposing (main)

import Dict
import Task exposing (Task)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Random
import WebGL exposing (Mesh, Shader)
import WebGL.Texture as Texture exposing (Texture)

import Animate exposing (Animated)
import Countries exposing (Countries, Country, GeoCoords)

type alias Flags = String

type alias GeneratingInitialCountryModel =
    { mapTexture : Maybe Texture
    , pageVisibility : Browser.Events.Visibility
    }

type alias LoadingInitialTexturesModel =
    { mapTexture : Maybe Texture
    , highlightTexture : Maybe Texture
    , pageVisibility : Browser.Events.Visibility
    , countries : Countries
    , country : Country
    }

type alias InitializedModel a =
    { a
    | mapTexture : Texture
    , highlightTexture : Texture
    , pageVisibility : Browser.Events.Visibility
    , countries : Countries
    , countryName : String
    , cameraDistance : Animated
    , cameraLatitude : Animated
    , cameraLongitude : Animated
    }

type alias AnsweringModel = InitializedModel
    { countryAlternativeNames : List String
    , answer : String
    }

type alias SubmittedModel a = InitializedModel { a | correct : Bool }

type alias DisplayResultsModel = SubmittedModel {}

type alias LoadingNewHighlightModel = SubmittedModel { newCountry : Country }

type Model
    = GeneratingInitialCountryState GeneratingInitialCountryModel
    | LoadingInitialTexturesState LoadingInitialTexturesModel
    | AnsweringState AnsweringModel
    | DisplayResultsState DisplayResultsModel
    | LoadingNewHighlightState LoadingNewHighlightModel
    | ErrorState

type Msg
    = GotMapTexture Texture
    | LoadMapTextureError
    | GotHighlightTextureUrl String
    | GotHighlightTexture Texture
    | LoadHighlightTextureError
    | GotCountries (Country, Countries)
    | GenerateCountryError -- this should never actually happen
    | AnswerUpdated String
    | SubmitButtonClicked
    | NextCountryButtonClicked
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

port fillCountryByCode : String -> Cmd msg
port drawDots : List Dot -> Cmd msg
port highlightTextureUrl : (String -> msg) -> Sub msg

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
 - texture loading
 - ---
 -}

loadTextureOptions : Texture.Options
loadTextureOptions =
    let
        defaultOptions = Texture.defaultOptions
    in
    -- can't use mipmap for minify since texture extremely distorted near poles
    { defaultOptions | minify = Texture.nearest, flipY = False }

loadTexture : String -> Task Texture.Error Texture
loadTexture = Texture.loadWith loadTextureOptions

loadMapTextureResult : Result Texture.Error Texture -> Msg
loadMapTextureResult result = case result of
    Ok texture -> GotMapTexture texture
    Err _ -> LoadMapTextureError

loadMapTexture : String -> Cmd Msg
loadMapTexture url = loadTexture url
    |> Task.attempt loadMapTextureResult

loadHighlightTextureResult : Result Texture.Error Texture -> Msg
loadHighlightTextureResult result = case result of
    Ok texture -> GotHighlightTexture texture
    Err _ -> LoadHighlightTextureError

loadHighlightTexture : String -> Cmd Msg
loadHighlightTexture url = loadTexture url
    |> Task.attempt loadHighlightTextureResult

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

{- ---
 - init
 - ---
 -}

init : Flags -> (Model, Cmd Msg)
init mapTextureUrl =
    ( GeneratingInitialCountryState
        { mapTexture = Nothing
        -- this is a guess, but no significant harm when we are wrong
        , pageVisibility = Browser.Events.Visible
        }
    , Cmd.batch
        [ loadMapTexture mapTextureUrl
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

tryFinishInitialization : LoadingInitialTexturesModel -> Model
tryFinishInitialization loadingModel =
    case (loadingModel.mapTexture, loadingModel.highlightTexture) of
        (Just mapTexture, Just highlightTexture) ->
            let
                { pageVisibility, countries, country } = loadingModel
            in AnsweringState
                { mapTexture = mapTexture
                , highlightTexture = highlightTexture
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
        _ -> LoadingInitialTexturesState loadingModel

checkAnswer : AnsweringModel -> Bool
checkAnswer { countryName, countryAlternativeNames, answer } =
    let
        lowerName = String.toLower countryName
        lowerAlternativeNames = List.map String.toLower countryAlternativeNames
        lowerAnswer = String.toLower answer
    in
    lowerAnswer == lowerName || List.member lowerAnswer lowerAlternativeNames

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
        GotMapTexture texture ->
            ( GeneratingInitialCountryState
                { generatingModel | mapTexture = Just texture }
            , Cmd.none
            )
        LoadMapTextureError -> (ErrorState, Cmd.none)
        GotHighlightTextureUrl _ -> (ErrorState, Cmd.none)
        GotHighlightTexture _ -> (ErrorState, Cmd.none)
        LoadHighlightTextureError -> (ErrorState, Cmd.none)
        GotCountries (country, countries) ->
            let
                { mapTexture, pageVisibility } = generatingModel
            in
            ( LoadingInitialTexturesState
                { mapTexture = mapTexture
                , highlightTexture = Nothing
                , pageVisibility = pageVisibility
                , countries = countries
                , country = country
                }
            , highlightCountry country
            )
        GenerateCountryError -> (ErrorState, Cmd.none)
        AnswerUpdated _ -> (ErrorState, Cmd.none)
        SubmitButtonClicked -> (ErrorState, Cmd.none)
        NextCountryButtonClicked -> (ErrorState, Cmd.none)
        PageVisibilityChange visibility ->
            ( GeneratingInitialCountryState
                { generatingModel | pageVisibility = visibility }
            , Cmd.none
            )
        AnimationFrame _ -> (ErrorState, Cmd.none)
    LoadingInitialTexturesState loadingModel -> case msg of
        GotMapTexture texture ->
            ( tryFinishInitialization
                { loadingModel | mapTexture = Just texture }
            , Cmd.none
            )
        LoadMapTextureError -> (ErrorState, Cmd.none)
        GotHighlightTextureUrl url -> (model, loadHighlightTexture url)
        GotHighlightTexture texture ->
            ( tryFinishInitialization
                { loadingModel | highlightTexture = Just texture }
            , Cmd.none
            )
        LoadHighlightTextureError -> (ErrorState, Cmd.none)
        GotCountries _ -> (ErrorState, Cmd.none)
        GenerateCountryError -> (ErrorState, Cmd.none)
        AnswerUpdated _ -> (ErrorState, Cmd.none)
        SubmitButtonClicked -> (ErrorState, Cmd.none)
        NextCountryButtonClicked -> (ErrorState, Cmd.none)
        PageVisibilityChange visibility ->
            ( LoadingInitialTexturesState
                { loadingModel | pageVisibility = visibility }
            , Cmd.none
            )
        AnimationFrame _ -> (ErrorState, Cmd.none)
    AnsweringState answeringModel -> case msg of
        GotMapTexture _ -> (ErrorState, Cmd.none)
        LoadMapTextureError -> (ErrorState, Cmd.none)
        GotHighlightTextureUrl _ -> (ErrorState, Cmd.none)
        GotHighlightTexture _ -> (ErrorState, Cmd.none)
        LoadHighlightTextureError -> (ErrorState, Cmd.none)
        GotCountries _ -> (ErrorState, Cmd.none)
        GenerateCountryError -> (ErrorState, Cmd.none)
        AnswerUpdated newAnswer ->
            ( AnsweringState { answeringModel | answer = newAnswer }
            , Cmd.none
            )
        SubmitButtonClicked ->
            let
                { mapTexture, highlightTexture, pageVisibility, countries,
                    countryName, cameraDistance, cameraLatitude,
                    cameraLongitude } = answeringModel
            in
            ( DisplayResultsState
                { mapTexture = mapTexture
                , highlightTexture = highlightTexture
                , pageVisibility = pageVisibility
                , countries = countries
                , countryName = countryName
                , cameraDistance = cameraDistance
                , cameraLatitude = cameraLatitude
                , cameraLongitude = cameraLongitude
                , correct = checkAnswer answeringModel
                }
            , Cmd.none
            )
        NextCountryButtonClicked -> (ErrorState, Cmd.none)
        PageVisibilityChange visibility ->
            ( AnsweringState { answeringModel | pageVisibility = visibility }
            , Cmd.none
            )
        AnimationFrame delta ->
            ( AnsweringState (updateAnimation delta answeringModel)
            , Cmd.none
            )
    DisplayResultsState resultsModel -> case msg of
        GotMapTexture _ -> (ErrorState, Cmd.none)
        LoadMapTextureError -> (ErrorState, Cmd.none)
        GotHighlightTextureUrl _ -> (ErrorState, Cmd.none)
        GotHighlightTexture _ -> (ErrorState, Cmd.none)
        LoadHighlightTextureError -> (ErrorState, Cmd.none)
        GotCountries (country, countries) ->
            let
                { mapTexture, highlightTexture, pageVisibility, countryName,
                    cameraDistance, cameraLatitude, cameraLongitude,
                    correct } = resultsModel
            in
            ( LoadingNewHighlightState
                { mapTexture = mapTexture
                , highlightTexture = highlightTexture
                , pageVisibility = pageVisibility
                , countries = countries
                , countryName = countryName
                , cameraDistance = cameraDistance
                , cameraLatitude = cameraLatitude
                , cameraLongitude = cameraLongitude
                , correct = correct
                , newCountry = country
                }
            , highlightCountry country
            )
        GenerateCountryError -> (ErrorState, Cmd.none)
        AnswerUpdated _ -> (ErrorState, Cmd.none)
        SubmitButtonClicked -> (ErrorState, Cmd.none)
        NextCountryButtonClicked ->
            let
                { countries } = resultsModel
            in
            ( DisplayResultsState resultsModel
            , Random.generate generateCountryResult (Countries.next countries)
            )
        PageVisibilityChange visibility ->
            ( DisplayResultsState
                { resultsModel | pageVisibility = visibility }
            , Cmd.none
            )
        AnimationFrame delta ->
            ( DisplayResultsState (updateAnimation delta resultsModel)
            , Cmd.none
            )
    LoadingNewHighlightState loadingModel -> case msg of
        GotMapTexture _ -> (ErrorState, Cmd.none)
        LoadMapTextureError -> (ErrorState, Cmd.none)
        GotHighlightTextureUrl url -> (model, loadHighlightTexture url)
        GotHighlightTexture highlightTexture ->
            let
                { mapTexture, cameraDistance, pageVisibility, countries,
                        cameraLatitude, cameraLongitude, newCountry } =
                    loadingModel
            in
            ( AnsweringState
                { mapTexture = mapTexture
                , highlightTexture = highlightTexture
                , pageVisibility = pageVisibility
                , countries = countries
                , countryName = newCountry.name
                , cameraDistance = Animate.to
                    (countryCameraDistance newCountry)
                    cameraDistance
                , cameraLatitude =
                    Animate.to newCountry.latitude cameraLatitude
                , cameraLongitude =
                    Animate.to newCountry.longitude cameraLongitude
                , countryAlternativeNames = newCountry.alternativeNames
                , answer = ""
                }
            , Cmd.none
            )
        LoadHighlightTextureError -> (ErrorState, Cmd.none)
        GotCountries _ -> (ErrorState, Cmd.none)
        GenerateCountryError -> (ErrorState, Cmd.none)
        AnswerUpdated _ -> (ErrorState, Cmd.none)
        SubmitButtonClicked -> (ErrorState, Cmd.none)
        NextCountryButtonClicked -> (ErrorState, Cmd.none)
        PageVisibilityChange visibility ->
            ( LoadingNewHighlightState
                { loadingModel | pageVisibility = visibility }
            , Cmd.none
            )
        AnimationFrame delta ->
            ( LoadingNewHighlightState (updateAnimation delta loadingModel)
            , Cmd.none
            )
    ErrorState -> (ErrorState, Cmd.none)

{- ---
 - subscriptions
 - ---
 -}

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
    GeneratingInitialCountryState _ ->
        Browser.Events.onVisibilityChange PageVisibilityChange
    LoadingInitialTexturesState _ -> Sub.batch
        [ highlightTextureUrl GotHighlightTextureUrl
        , Browser.Events.onVisibilityChange PageVisibilityChange
        ]
    AnsweringState answeringModel -> Sub.batch
        [ Browser.Events.onVisibilityChange PageVisibilityChange
        , animationSubscriptions answeringModel
        ]
    DisplayResultsState resultsModel -> Sub.batch
        [ Browser.Events.onVisibilityChange PageVisibilityChange
        , animationSubscriptions resultsModel
        ]
    LoadingNewHighlightState loadingModel -> Sub.batch
        [ highlightTextureUrl GotHighlightTextureUrl
        , Browser.Events.onVisibilityChange PageVisibilityChange
        , animationSubscriptions loadingModel
        ]
    ErrorState -> Sub.none

{- ---
 - view
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

triangles : List Triangle
triangles = northTriangles ++ southTriangles

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

mesh : Mesh Vertex
mesh = triangles ++ List.concatMap quadTriangles quads |> WebGL.triangles

type alias Uniforms =
    { rotation : Mat4
    , camera : Mat4
    , perspective : Mat4
    , mapTexture : Texture
    , highlightTexture : Texture
    , ambientBrightness : Float
    , diffuseBrightness : Float
    , specularBrightness : Float
    , lightColor : Vec3
    , lightDir : Vec3
    , cameraPos : Vec3
    , shininess : Float
    }

type alias Varying =
    { vTextureCoord : Vec2
    , vPos : Vec3
    }

white : Vec3
white = vec3 1 1 1

uniforms : InitializedModel a -> Uniforms
uniforms { mapTexture, highlightTexture, cameraDistance, cameraLatitude,
        cameraLongitude } =
    let
        cameraPos = Vec3.scale
            (1 + Animate.get cameraDistance)
            (Vec3.negate Vec3.i)
    in
    { rotation =
        Mat4.makeRotate
            (Animate.get cameraLatitude |> degrees |> negate)
            Vec3.j
        |> Mat4.rotate
            (Animate.get cameraLongitude |> degrees |> negate)
            Vec3.k
    , camera = Mat4.makeLookAt cameraPos (vec3 0 0 0) Vec3.k
    , perspective = Mat4.makePerspective 45 1 0.01 100
    , mapTexture = mapTexture
    , highlightTexture = highlightTexture
    , ambientBrightness = 0.5
    , diffuseBrightness = 0.5
    , specularBrightness = 0.1
    , lightColor = white
    , lightDir = Vec3.normalize <| vec3 -2 -1 1
    , cameraPos = cameraPos
    , shininess = 2
    }

vertexShader : Shader Vertex Uniforms Varying
vertexShader = [glsl|
        attribute vec3 pos;
        attribute vec2 textureCoords;
        uniform mat4 rotation;
        uniform mat4 camera;
        uniform mat4 perspective;
        varying vec2 vTextureCoord;
        varying vec3 vPos;
        void main() {
            gl_Position = perspective * camera * rotation * vec4(pos, 1.0);
            vTextureCoord = textureCoords;
            vPos = mat3(rotation) * pos;
        }
    |]

fragmentShader : Shader {} Uniforms Varying
fragmentShader = [glsl|
        precision lowp float;
        uniform sampler2D mapTexture;
        uniform sampler2D highlightTexture;
        uniform float ambientBrightness;
        uniform float diffuseBrightness;
        uniform float specularBrightness;
        uniform vec3 lightColor;
        uniform vec3 lightDir;
        uniform vec3 cameraPos;
        uniform float shininess;
        varying vec2 vTextureCoord;
        varying vec3 vPos;

        vec4 sampleTextures() {
            vec4 mapColor = texture2D(mapTexture, vTextureCoord);
            vec4 highlightColor = texture2D(highlightTexture, vTextureCoord);
            float alpha = highlightColor.w
                + mapColor.w * (1.0 - highlightColor.w);
            vec3 rgb =
                (highlightColor.xyz * highlightColor.w
                    + mapColor.xyz * mapColor.w * (1.0 - highlightColor.w))
                / alpha;
            return vec4(rgb, alpha);
        }

        void main() {
            vec3 ambient = ambientBrightness * lightColor;
            // normal is same as position since unit sphere centered at origin
            vec3 normal = vPos;
            vec3 diffuse = diffuseBrightness
                * max(dot(normal, lightDir), 0.0)
                * lightColor;
            vec3 cameraDir = normalize(cameraPos - vPos);
            vec3 reflectDir = reflect(-lightDir, normal);
            vec3 specular = specularBrightness
                * pow(max(dot(cameraDir, reflectDir), 0.0), shininess)
                * lightColor;
            vec3 lighting = ambient + diffuse + specular;
            vec4 objColor = sampleTextures();
            gl_FragColor = vec4(lighting, 1.0) * objColor;
        }
    |]

viewGraphics : InitializedModel a -> Html msg
viewGraphics model = WebGL.toHtml
    [ Attrs.width 800
    , Attrs.height 800
    , Attrs.style "width" "400px"
    , Attrs.style "height" "400px"
    , Attrs.style "display" "block"
    , Attrs.style "background-color" "#2e4482"
    ]
    [ WebGL.entity vertexShader fragmentShader mesh (uniforms model) ]

viewUiContainer : List (Html msg) -> Html msg
viewUiContainer contents = Html.div [ Attrs.style "padding" "12px" ] contents

viewAnswering : AnsweringModel -> List (Html Msg)
viewAnswering { answer } =
    [ Html.div [] [ Html.text "What country is this?" ]
    , Html.div
        [ Attrs.style "padding-top" "8px" ]
        [ Html.input
            [ Attrs.placeholder "Answer"
            , Attrs.value answer
            , Html.Events.onInput AnswerUpdated
            ]
            []
        , Html.span
            [ Attrs.style "padding-left" "4px" ]
            [ Html.button
                [ Html.Events.onClick SubmitButtonClicked ]
                [ Html.text "Submit" ]
            ]
        ]
    ]

viewSubmitted : Bool -> SubmittedModel a -> List (Html Msg)
viewSubmitted nextCountryButtonEnabled { correct } =
    let
        text = case correct of
            True -> "Correct!"
            False -> "Incorrect..."
    in
    [ Html.div [] [ Html.text text ]
    , Html.div
        [ Attrs.style "padding-top" "8px" ]
        [ Html.button
            [ Attrs.disabled (not nextCountryButtonEnabled)
            , Html.Events.onClick NextCountryButtonClicked
            ]
            [ Html.text "Continue" ]
        ]
    ]

view : Model -> Html Msg
view model = case model of
    GeneratingInitialCountryState _ -> Html.div [] []
    LoadingInitialTexturesState _ -> Html.div [] []
    AnsweringState answeringModel -> Html.div
        []
        [ viewGraphics answeringModel
        , viewUiContainer (viewAnswering answeringModel)
        ]
    DisplayResultsState resultsModel -> Html.div
        []
        [ viewGraphics resultsModel
        , viewUiContainer (viewSubmitted True resultsModel)
        ]
    LoadingNewHighlightState loadingModel -> Html.div
        []
        [ viewGraphics loadingModel
        , viewUiContainer (viewSubmitted False loadingModel)
        ]
    ErrorState -> Html.div [] []

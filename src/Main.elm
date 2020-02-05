module Main exposing (main)

import Angle
import Axis2d
import BoundingBox2d
import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events as Events
import Element exposing (Element)
import Element.Background as Background
import Element.Input as Input
import Geometry.Svg as Svg
import LSystem
import LSystem.Rules as Rules
import Pixels exposing (Pixels)
import Point2d
import Polyline2d
import Quantity exposing (Quantity)
import Svg
import Svg.Attributes as SvgAttr
import Task
import Turtle


type alias LSystem a =
    { a
        | iteration : Int
        , angle : Float
        , axiom : String
        , rules : LSystem.Productions
        , minIteration : Int
        , maxIteration : Int
    }


type alias Model =
    LSystem
        { viewportWidth : Int
        , viewportHeight : Int
        }


type Orientation
    = Portrait
    | Landscape


type Msg
    = SetIteration Int
    | LoadPlant
    | LoadDragon
    | LoadLevy
    | Viewport Int Int


length : Quantity Float Pixels
length =
    Pixels.pixels 10


dragon : LSystem a -> LSystem a
dragon lsystem =
    let
        rules =
            Rules.empty
                |> Rules.add 'X' "X+YF+"
                |> Rules.add 'Y' "-FX-Y"
    in
    { lsystem
        | iteration = 5
        , angle = 90
        , axiom = "FX"
        , rules = rules
        , minIteration = 0
        , maxIteration = 10
    }


levyC : LSystem a -> LSystem a
levyC lsystem =
    let
        rules =
            Rules.empty
                |> Rules.add 'F' "+F--F+"
    in
    { lsystem
        | iteration = 5
        , angle = 45
        , axiom = "F"
        , rules = rules
        , minIteration = 0
        , maxIteration = 10
    }


plant : LSystem a -> LSystem a
plant lsystem =
    let
        rules =
            Rules.empty
                |> Rules.add 'X' "F+[[X]-X]-F[-FX]+X"
                |> Rules.add 'F' "FF"
    in
    { lsystem
        | iteration = 3
        , angle = 25
        , axiom = "X"
        , rules = rules
        , minIteration = 1
        , maxIteration = 5
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( dragon
        { iteration = 0
        , angle = 0
        , axiom = ""
        , rules = Rules.empty
        , minIteration = 0
        , maxIteration = 0
        , viewportWidth = 800
        , viewportHeight = 600
        }
    , Task.perform
        (\window ->
            Viewport
                (floor window.viewport.width)
                (floor window.viewport.height)
        )
        Dom.getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onResize Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        SetIteration iteration ->
            { model
                | iteration =
                    clamp
                        model.minIteration
                        model.maxIteration
                        iteration
            }

        LoadDragon ->
            dragon model

        LoadLevy ->
            levyC model

        LoadPlant ->
            plant model

        Viewport width height ->
            { model
                | viewportWidth = width
                , viewportHeight = height
            }
    , Cmd.none
    )


lightBlue : Element.Color
lightBlue =
    Element.rgb 0.8 0.96 1.0


sliderMin : Element.Color
sliderMin =
    lightBlue


sliderMax : Element.Color
sliderMax =
    Element.rgb 0.3 0.0 1.0


padding : Int
padding =
    10


spacing : Int
spacing =
    10


responsivePortion : Int -> Orientation -> Int -> Int -> List (Element.Attribute Msg)
responsivePortion portion orientation viewportWidth viewportHeight =
    case orientation of
        Portrait ->
            [ Element.width <| Element.px <| viewportWidth
            , Element.height <| Element.fillPortion portion
            ]

        Landscape ->
            [ Element.width <| Element.fillPortion portion
            , Element.height <| Element.px <| viewportHeight
            ]


button : Msg -> String -> Element Msg
button msg label =
    Input.button
        [ Background.color lightBlue
        , Element.padding padding
        ]
        { onPress = Just msg
        , label = Element.text label
        }


controls : Model -> Orientation -> Element Msg
controls model orientation =
    Element.column
        ([ Element.alignTop
         , Element.padding padding
         , Element.spacing spacing
         ]
            ++ responsivePortion 1
                orientation
                model.viewportWidth
                model.viewportHeight
        )
        [ Input.slider
            [ Background.gradient
                { angle = pi / 2
                , steps = [ sliderMin, sliderMax ]
                }
            ]
            { onChange = round >> SetIteration
            , label = Element.text "Steps" |> Input.labelLeft []
            , min = toFloat model.minIteration
            , max = toFloat model.maxIteration
            , value = toFloat model.iteration
            , thumb = Input.defaultThumb
            , step = Just 1
            }
        , Element.row [ Element.spacing spacing ]
            [ button LoadDragon "Dragon curve"
            , button LoadLevy "Lévy C curve"
            , button LoadPlant "Plant"
            ]
        ]


svg : Model -> Orientation -> Element Msg
svg model orientation =
    let
        commands =
            LSystem.iteration model.rules model.axiom model.iteration

        drawing =
            LSystem.commands length (Angle.degrees model.angle) commands
                |> Turtle.draw
                |> List.map (Polyline2d.mirrorAcross Axis2d.x)

        boundingBox =
            Turtle.boundingBox drawing
                |> Maybe.withDefault
                    (BoundingBox2d.from
                        (Point2d.pixels -400 -300)
                        (Point2d.pixels 400 300)
                    )

        viewBox =
            Turtle.viewBox length boundingBox
    in
    Svg.svg
        [ SvgAttr.viewBox viewBox ]
        [ List.map (Svg.polyline2d [])
            drawing
            |> Svg.g
                [ SvgAttr.stroke "green"
                , SvgAttr.fill "none"
                , SvgAttr.strokeLinejoin "round"
                , SvgAttr.strokeLinecap "round"
                ]
        ]
        |> Element.html
        |> Element.el
            ([ Element.centerX
             , Element.centerY
             , Element.padding padding
             ]
                ++ responsivePortion 3
                    orientation
                    model.viewportWidth
                    model.viewportHeight
            )


view : Model -> Document Msg
view model =
    let
        orientation =
            if model.viewportHeight > model.viewportWidth then
                Portrait

            else
                Landscape

        fill =
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
    in
    { title = "L-systems in Elm"
    , body =
        [ Element.layout [] <|
            case orientation of
                Portrait ->
                    Element.column fill
                        [ controls model orientation
                        , svg model orientation
                        ]

                Landscape ->
                    Element.row fill
                        [ svg model orientation
                        , controls model orientation
                        ]
        ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

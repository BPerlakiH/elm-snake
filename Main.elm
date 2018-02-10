module Main exposing (Model, init, subscriptions, update, view)

import Collage
import Color exposing (Color)
import Element
import Html exposing (..)
import Html.Attributes exposing (style)
import Keyboard exposing (KeyCode)
import Random exposing (Generator, generate, int, pair)
import Time exposing (Time)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (Time.inMilliseconds 40) Tick
        , Keyboard.downs onKey
        ]


onKey : KeyCode -> Msg
onKey code =
    case code of
        37 ->
            --left
            Move ( -1, 0 )

        39 ->
            --right
            Move ( 1, 0 )

        40 ->
            --down
            Move ( 0, -1 )

        38 ->
            --up
            Move ( 0, 1 )

        32 ->
            --stop
            Move ( 0, 0 )

        _ ->
            OtherKey code


type alias Vector =
    ( Int, Int )


type Msg
    = Tick Time
    | Move Vector
    | OtherKey KeyCode
    | RepositionApple
    | NewApplePos Vector


type alias Model =
    { keycode : String
    , direction : Vector
    , headPos : Vector
    , trail : List Vector
    , tailLength : Int
    , applePos : Vector
    }


boardWidth : Int
boardWidth =
    50


boardHeight : Int
boardHeight =
    40


gridSize : Int
gridSize =
    20


model : Model
model =
    { keycode = "init"
    , direction = ( 0, 0 )
    , headPos = ( boardWidth // 2, boardHeight // 2 )
    , trail = []
    , tailLength = 5
    , applePos = ( boardWidth // 2, boardHeight // 2 )
    }


randomBoardPoint : Generator Vector
randomBoardPoint =
    pair (int 0 (boardWidth - 1)) (int 0 (boardHeight - 1))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            model
                |> move
                |> isUpdate

        Move vector ->
            updateDirection model vector

        OtherKey code ->
            ( model, Cmd.none )

        RepositionApple ->
            ( model, generate NewApplePos randomBoardPoint )

        NewApplePos ( x, y ) ->
            ( { model | applePos = ( x, y ) }, Cmd.none )


updateDirection : Model -> Vector -> ( Model, Cmd Msg )
updateDirection model newDirection =
    ( { model | direction = newDirection }, Cmd.none )


move : Model -> Model
move model =
    model |> moveSnake


isUpdate : Model -> ( Model, Cmd Msg )
isUpdate model =
    if snakeMeetsApple model then
        ( { model | tailLength = model.tailLength + 1 }, generate NewApplePos randomBoardPoint )
    else
        ( model, Cmd.none )


snakeMeetsApple : Model -> Bool
snakeMeetsApple model =
    List.member model.applePos model.trail


moveSnake : Model -> Model
moveSnake model =
    let
        defPos =
            ( boardWidth // 2, boardHeight // 2 )

        newHead : Vector
        newHead =
            newPosition model.headPos model.direction

        newList : List Vector
        newList =
            List.take model.tailLength (newHead :: model.trail)
    in
    { model | headPos = newHead, trail = newList }


newPosition : Vector -> Vector -> Vector
newPosition pos delta =
    let
        ( dx, dy ) =
            delta

        ( x, y ) =
            pos

        ( newX, newY ) =
            ( x + dx, y + dy )
    in
    ( newX % boardWidth, newY % boardHeight )


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html Msg
view model =
    let
        widthString =
            toString (boardWidth * gridSize) ++ "px"

        heightString =
            toString (boardHeight * gridSize) ++ "px"
    in
    div
        [ style [ ( "padding", "30px" ) ]
        ]
        [ div
            [ style
                [ ( "width", widthString )
                , ( "height", heightString )
                , ( "margin", "auto" )
                , ( "position", "relative" )
                ]
            ]
            [ renderSnake model

            -- , div [] [ text ("snake: " ++ toString model.trail) ]
            -- , div [] [ text ("apple: " ++ toString model.applePos) ]
            ]
        ]


drawRect : Int -> Int -> Color -> Collage.Form
drawRect width height color =
    Collage.rect (toFloat width) (toFloat height)
        |> Collage.filled color


renderSnake : Model -> Html Msg
renderSnake model =
    let
        ( width, height ) =
            ( boardWidth * gridSize, boardHeight * gridSize )

        snakeParts =
            List.map renderSnakeUnit model.trail

        board =
            drawRect width height (Color.rgb 36 41 44)

        apple =
            renderUnit model.applePos (Color.rgb 158 217 74)
    in
    List.concat [ [ board, apple ], snakeParts ]
        |> Collage.collage width height
        |> Element.toHtml


toFloatGridPos : Int -> Float
toFloatGridPos value =
    toFloat ((value * gridSize) + gridSize // 2)


toRenderPos : Vector -> ( Float, Float )
toRenderPos vector =
    let
        ( x, y ) =
            vector

        xOff =
            (0 - boardWidth) // 2

        yOff =
            (0 - boardHeight) // 2
    in
    ( toFloatGridPos (x + xOff), toFloatGridPos (y + yOff) )


renderUnit : Vector -> Color -> Collage.Form
renderUnit atPosition paintColor =
    let
        ( x, y ) =
            toRenderPos atPosition
    in
    drawRect (gridSize - 1) (gridSize - 1) paintColor
        |> Collage.move ( x + 1, y + 1 )


renderSnakeUnit : Vector -> Collage.Form
renderSnakeUnit atPosition =
    renderUnit atPosition (Color.rgb 23 189 255)

-- Author: Marcus Adair, CS6525, Spring 2024, University of Utah


module Main exposing (main)

import Basics exposing (..)
import Browser
import Browser.Events exposing (onKeyDown)
import Debug exposing (log)
import Html exposing (Html, div, span, strong, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, field, map, string)
import Random exposing (Generator, int, step, weighted)



------------------------------------------------------------------
-- Type for game model:
------------------------------------------------------------------


type alias Tile =
    -- A Tile on the boards is an Int (0 is empty)
    Int


type alias Row =
    -- A row is a list of 4 tiles
    List Tile


type alias Grid =
    -- A Grid is 4 rows
    List Row


gridSize : Int
gridSize =
    4



-- Model for the game


type alias GameState =
    { grid : Grid
    , score : Int
    , won : Bool
    , lost : Bool
    , overlayDisplayed : Bool
    , bestScore : Int
    }



-- Check if two Tiles are equal


tileEqual : Tile -> Tile -> Bool
tileEqual tile1 tile2 =
    tile1 == tile2



-- Check if two Rows are equal


rowEqual : Row -> Row -> Bool
rowEqual row1 row2 =
    case ( row1, row2 ) of
        ( [], [] ) ->
            -- return true when end is reached
            True

        -- Check for tile equality & recurse on the rest
        ( tile1 :: rest1, tile2 :: rest2 ) ->
            tileEqual tile1 tile2 && rowEqual rest1 rest2

        _ ->
            False



-- Check if two Grids are equal


gridEqual : Grid -> Grid -> Bool
gridEqual grid1 grid2 =
    case ( grid1, grid2 ) of
        ( [], [] ) ->
            -- return true when end is reached
            True

        -- Check for row equality & recurse on the rest
        ( row1 :: rest1, row2 :: rest2 ) ->
            rowEqual row1 row2 && gridEqual rest1 rest2

        _ ->
            False


emptyGrid : Grid
emptyGrid =
    [ [ 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0 ]
    ]



-- Initialize the grid at the start of a game


initGrid : Random.Seed -> Grid
initGrid seed =
    let
        -- Generate random row and column indices
        ( randomRow, seed1 ) =
            generateRandomIdx seed

        ( randomCol, seed2 ) =
            generateRandomIdx seed1

        -- Generate 2 or 4 to spawn on emtpy grid
        ( twoOrFour, _ ) =
            generateRanTileNum seed2

        -- place starting tile in random spot
        gridWithTile =
            setTile emptyGrid randomRow randomCol twoOrFour
    in
    gridWithTile



-- Initial seeding for randomness


initialSeed : Random.Seed
initialSeed =
    Random.initialSeed 69



-- Set the initial seed


randomSeed : Int -> Random.Seed
randomSeed seed =
    Random.initialSeed seed



-------------------------------------------------
-- MAIN
-------------------------------------------------


main : Program () GameState Msg
main =
    let
        -- Initialize grid
        grid =
            initGrid initialSeed

        -- Setup GameState
        newGame =
            { grid = grid
            , score = 0
            , won = False
            , lost = False
            , overlayDisplayed = False
            , bestScore = 0
            }
    in
    Browser.element
        { -- Initialize the game model
          init = \_ -> ( newGame, Cmd.none )

        -- Update game model based on incoming messages
        , update = update

        -- Show things on screen, render HTMl
        , view = view

        -- Set method to subsribe to extern events (key presses) & respond
        , subscriptions = subscriptions
        }



------------------------------------------------------------------
-- Game Input
------------------------------------------------------------------


subscriptions : GameState -> Sub Msg
subscriptions g =
    if g.overlayDisplayed then
        Sub.none
        -- disable gameplay when overlay is displayed

    else
        onKeyDown arrowKeyDecoder



-- decode incoming arrow key
-- Define custom messages to be sent in the app for making updates


type Msg
    = MoveLeft
    | MoveRight
    | MoveUp
    | MoveDown
    | NewGame



-- Method to decode


arrowKeyDecoder : Decoder Msg
arrowKeyDecoder =
    -- Decode JSON from key press to string
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            -- Emit a message based off of the key press
            (\key ->
                case key of
                    "ArrowLeft" ->
                        Json.Decode.succeed MoveLeft

                    "ArrowRight" ->
                        Json.Decode.succeed MoveRight

                    "ArrowUp" ->
                        Json.Decode.succeed MoveUp

                    "ArrowDown" ->
                        Json.Decode.succeed MoveDown

                    _ ->
                        Json.Decode.fail "Not an Arrow Key"
            )



-- Method that reacts to messages and updates model


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg state =
    let
        ( ranNum, seed ) =
            generateRandomIdx initialSeed

        -- Set new seed to update randomness
        newSeed =
            randomSeed ranNum

        best =
            state.bestScore
    in
    -- update according to messages
    case msg of
        MoveLeft ->
            ( slideLeft state newSeed, Cmd.none )

        MoveRight ->
            ( slideRight state newSeed, Cmd.none )

        MoveUp ->
            ( slideUp state newSeed, Cmd.none )

        MoveDown ->
            ( slideDown state newSeed, Cmd.none )

        -- on New Game button press
        NewGame ->
            ( resetGame state best, Cmd.none )



-- Reset GameState model after new button pressed


resetGame : GameState -> Int -> GameState
resetGame state best =
    { grid = initGrid initialSeed, score = 0, won = False, lost = False, overlayDisplayed = False, bestScore = best }



--------------------------------------------------
-- Game View
--------------------------------------------------
-- Render the game into HTML after updates


view : GameState -> Html Msg
view state =
    let
        -- extract vars from the state
        grid =
            state.grid

        score =
            state.score

        bestScore =
            state.bestScore

        won =
            state.won

        lost =
            state.lost

        scoreStr =
            String.fromInt score

        bestScoreStr =
            String.fromInt bestScore

        -- map the grid to rendered tiles
        gridRows =
            grid
                |> List.map
                    (\row ->
                        div [ style "display" "flex" ]
                            (row |> List.map renderTile)
                    )

        -- Overlay text for when game is won or lost
        overlay =
            if won then
                Just "GAME WON!"

            else if lost then
                Just "GAME LOST!"

            else
                Nothing
    in
    div
        -- Main div for whole page
        [ style "width" "100%"
        , style "height" "100vh"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "background-color" "rgb(250 248 239)"
        , style "padding" "10px"
        ]
        [ div
            -- Score Div
            [ style "width" "150px"
            , style "height" "50px"
            , style "background-color" "rgb(188 172 159)"
            , style "color" "white"
            , style "font-size" "20px"
            , style "font-family" "Helvetica Neue, Arial, sans-serif"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "margin-left" "300px"
            , style "color" "rgb(238 228 218)"
            , style "border-radius" "2px"
            ]
            [ text "SCORE: "
            , span
                [ style "font-weight" "bold"
                , style "color" "white"
                ]
                [ text scoreStr ]
            ]
        , div
            -- Best Score Div
            [ style "width" "150px"
            , style "height" "50px"
            , style "background-color" "rgb(188 172 159)"
            , style "color" "white"
            , style "font-size" "20px"
            , style "font-family" "Helvetica Neue, Arial, sans-serif"
            , style "display" "flex"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "margin-left" "300px"
            , style "margin-top" "5px"
            , style "color" "rgb(238 228 218)"
            , style "border-radius" "2px"
            ]
            [ text "BEST:  "
            , span
                [ style "font-weight" "bold"
                , style "color" "white"
                ]
                [ text bestScoreStr ]
            ]

        -- Instruction text
        , span
            [ style "margin-bottom" "10px"
            , style "margin-right" "150px"
            , style "color" "rgb(143 122 101)"
            , style "font-size" "18px"
            , style "font-family" "Helvetica Neue, Arial, sans-serif"
            ]
            [ span [ style "font-weight" "bold", style "font-size" "80px", style "color" "rgb(119, 110, 101)" ] [ text "2048" ]
            , text " "
            , Html.br [] []
            , text "Join numbers to get to the "
            , span [ style "font-weight" "bold" ] [ text "2048 tile!" ]
            ]

        -- New Game button
        , Html.button
            [ onClick NewGame -- Emit a new game message on click of button
            , style "margin-left" "330px"
            , style "margin-bottom" "10px"
            , style "width" "120px"
            , style "height" "40px"
            , style "background-color" "rgb(143 122 101)"
            , style "border" "none"
            , style "border-radius" "2px"
            , style "color" "white"
            , style "font-size" "16px"
            , style "font-family" "Helvetica Neue, Arial, sans-serif"
            ]
            [ text "New Game" ]
        , div
            -- Border div behind tiles
            [ style "width" "440px"
            , style "height" "440px"
            , style "border" "5px solid #bbada0"
            , style "border-radius" "5px"
            , style "display" "flex"
            , style "flex-wrap" "wrap"
            , style "justify-content" "center"
            , style "align-items" "center"
            , style "background-color" "rgb(188 172 159)"
            ]
            -- Render the the tiles as a grid
            (gridRows
                -- Overlay on win/loss
                ++ (case overlay of
                        Just message ->
                            [ div
                                [ style "position" "absolute"
                                , style "z-index" "1"
                                , style "background-color" "rgba(0, 0, 0, 0.5)"
                                , style "color" "white"
                                , style "font-size" "24px"
                                , style "padding" "20px"
                                ]
                                [ text message ]
                            ]

                        Nothing ->
                            []
                   )
            )
        , span
            [ style "margin-top" "40px"
            , style "margin-right" "150px"
            , style "color" "rgb(119, 110, 101)"
            , style "font-size" "18px"
            , style "width" "440px"

            -- , style "display" "flex"
            -- , style "flex-wrap" "wrap"
            -- , style "justify-content" "center"
            , style "align-items" "center"
            , style "margin-left" "180px"
            , style "font-family" "Helvetica Neue, Arial, sans-serif"
            ]
            [ span [ style "font-weight" "bold" ]
                [ text "HOW TO PLAY:" ]
            , span []
                [ text
                    " Use your "
                ]
            , span
                [ style "font-weight" "bold" ]
                [ text
                    " arrow keys "
                ]
            , span [] [ text " to move the tiles. Tiles with the same number " ]
            , span
                [ style "font-weight" "bold" ]
                [ text
                    " merge into one "
                ]
            , span []
                [ text
                    "when they touch. Add them up to reach  "
                ]
            , span [ style "font-weight" "bold" ] [ text "2048!" ]
            ]
        ]



-- Styling for a single tile


renderTile : Tile -> Html Msg
renderTile tile =
    let
        tileText =
            if tile > 0 then
                text (String.fromInt tile)

            else
                text ""

        tileTextColor =
            if tile >= 8 then
                "rgb(249 246 242)"
                -- light text

            else
                -- dark text
                "rgb(119 110 101)"

        tileFontSize =
            if tile >= 1024 then
                "35px"

            else
                "45px"
    in
    div
        [ style "width" "100px" -- width of square
        , style "height" "100px" -- height of square
        , style "background-color" (tileColor tile)
        , style "margin" "5px" -- space between tiles
        , style "display" "flex"

        -- center content
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "font-size" tileFontSize
        , style "border-radius" "5px"
        , style "font-family" "Helvetica Neue, Arial, sans-serif"
        , style "font-weight" "bold"
        , style "color" tileTextColor
        ]
        [ tileText ]



-- Method to return the color of a given tile


tileColor : Tile -> String
tileColor tile =
    case tile of
        0 ->
            "rgba(205 192 180)"

        2 ->
            "rgb(238 228 218)"

        4 ->
            "rgb(237 224 200)"

        8 ->
            "rgb(242 177 121)"

        16 ->
            "rgb(245 149 99)"

        32 ->
            "rgb(246 124 95)"

        64 ->
            "rgb(246 94 59)"

        128 ->
            "rgb(237 207 11)"

        256 ->
            "rgb(237 204 97)"

        512 ->
            "rgb(237 200 80)"

        1024 ->
            "rgb(237 197 6)"

        2048 ->
            "rgb(237 194 4)"

        _ ->
            "rgb(238 228 21)"



--------------------------------------------------
-- Sliding Methods
--------------------------------------------------
-- Rotate grid by transposing and reversing rows


rotateGridClockwise : Grid -> Grid
rotateGridClockwise grid =
    grid
        |> transposeGrid
        |> List.map List.reverse



-- Transpose the Grid. Flip along diagonal axis, or essentially turn
-- Each column into a row


transposeGrid : Grid -> Grid
transposeGrid grid =
    case grid of
        [] ->
            []

        row :: rows ->
            case row of
                [] ->
                    transposeGrid rows

                _ ->
                    let
                        -- Extract the heads of each row into a list
                        headColumn =
                            filterNothings (List.map List.head grid)

                        -- The remainders of each row
                        tailColumns =
                            filterNothings (List.map List.tail grid)
                    in
                    -- Add the new row to be in the returned grid and recurse
                    headColumn :: transposeGrid tailColumns



-- Helper method to remove the 'Nothing' elemetns from a list of
-- Maybe a's and turn it to a list of a's


filterNothings : List (Maybe a) -> List a
filterNothings listWithMaybes =
    List.filterMap identity listWithMaybes



-- Takes in a row of non-empty tile, returns a list of sublists where
-- the sublists contain 1 or 2 tiles. If they contain 2 tiles, those
-- tiles will be equal and are to be merged


groupTilesForMerging : Row -> List (List Tile)
groupTilesForMerging row =
    case row of
        -- Single tile in row
        [ tile ] ->
            [ [ tile ] ]

        -- At Least two tiles in a row
        tile1 :: tile2 :: otherTiles ->
            if tile1 == tile2 then
                -- Group matching tiles and recurse on rest of the list
                [ tile1, tile2 ] :: groupTilesForMerging otherTiles

            else
                -- single out first tile and recurse on rest of the list
                [ tile1 ] :: groupTilesForMerging (tile2 :: otherTiles)

        _ ->
            []



-- Method for merging two tiles together


mergeTiles : List Tile -> ( Tile, Int )
mergeTiles tiles =
    case tiles of
        -- If single tile given, return it
        [ tile ] ->
            ( tile, 0 )

        -- If two tiles given, return their sum
        [ tile1, tile2 ] ->
            if tile1 == tile2 then
                let
                    sum =
                        tile1 + tile2
                in
                -- Score gained is the sum of the merged tiles
                ( sum, sum )

            else
                -- This shouldn't happen
                ( tile1, 0 )

        -- Empty case
        _ ->
            ( 0, 0 )



-- Method for sliding a single row to the left
-- Separate the list into two lists of the first and second components


separateList : List ( a, b ) -> ( List a, List b )
separateList list =
    let
        -- Function to extract the first and second components from a tuple
        extractComponents : ( a, b ) -> ( List a, List b ) -> ( List a, List b )
        extractComponents ( first, second ) ( accFirst, accSecond ) =
            -- Extract elements and put them into their own accumulator list
            ( first :: accFirst, second :: accSecond )
    in
    -- Use List.foldr to accumulate the first and second components into separate lists
    List.foldr extractComponents ( [], [] ) list



-- Helper method to sum ints in a List


sumInts : List Int -> Int
sumInts list =
    List.foldl (+) 0 list



-- Method to slide a single row to the left. Returns
-- the slid row and an int representing score accumulated


slideRow : Row -> ( Row, Int )
slideRow row =
    let
        -- Filter out the empty tiles
        nonEmptyTiles =
            List.filter (\tile -> tile /= 0) row

        -- Create sublists of matching tiles to merge
        groupedNonEmptyTiles =
            groupTilesForMerging nonEmptyTiles

        -- The count of tile after merging
        numTilesAfterMerge =
            List.length groupedNonEmptyTiles

        -- Merge tiles and get score
        listTileAndScore =
            List.map mergeTiles groupedNonEmptyTiles

        -- Separate slid/merged tiles and score
        ( listTile, listScores ) =
            separateList listTileAndScore

        -- Add 0s to the end of the slid tiles
        slidTiles =
            listTile ++ List.repeat (gridSize - numTilesAfterMerge) 0

        -- Sum the scores from merging
        summedScore =
            sumInts listScores
    in
    ( slidTiles, summedScore )



-- Method to slide an entire grid


slideGrid : Grid -> ( Grid, Int )
slideGrid grid =
    let
        -- For each row, slide elements in Grid to the left
        ( slidGrid, scoreFromSlide ) =
            List.foldl
                -- function to do foldl with
                (\row ( accGrid, accScore ) ->
                    let
                        ( slidRow, rowScore ) =
                            slideRow row
                    in
                    ( accGrid ++ [ slidRow ], accScore + rowScore )
                )
                -- initial accuulators
                ( [], 0 )
                -- List to do foldL on
                grid
    in
    ( slidGrid, scoreFromSlide )


slideLeft : GameState -> Random.Seed -> GameState
slideLeft state seed =
    let
        -- slide grid
        ( slidGrid, accScore ) =
            slideGrid state.grid

        -- add score gained
        newScore =
            state.score + accScore

        -- check for win/loss
        won =
            gameWon slidGrid

        lost =
            gameLost slidGrid

        overlay =
            won || lost

        best =
            if newScore > state.bestScore then
                newScore

            else
                state.bestScore

        slidGridWithNewTile =
            -- If grid is same after the slide, dont spawn new tile
            if gridEqual state.grid slidGrid then
                { grid = slidGrid, score = newScore, won = won, lost = lost, overlayDisplayed = overlay, bestScore = best }

            else
                -- spawn random tile
                { grid = spawnRandomTile slidGrid seed, score = newScore, won = won, lost = lost, overlayDisplayed = overlay, bestScore = best }
    in
    slidGridWithNewTile


slideDown : GameState -> Random.Seed -> GameState
slideDown state seed =
    let
        -- slide grid
        ( slidGrid_, accScore ) =
            state.grid
                |> rotateGridClockwise
                |> slideGrid

        -- add gained score
        newScore =
            state.score + accScore

        slidGrid =
            slidGrid_
                |> rotateGridClockwise
                |> rotateGridClockwise
                |> rotateGridClockwise

        -- check for win/loss
        won =
            gameWon slidGrid

        lost =
            gameLost slidGrid

        overlay =
            won || lost

        best =
            if newScore > state.bestScore then
                newScore

            else
                state.bestScore

        slidGridWithNewTile =
            -- If grid is same after the slide, dont spawn new tile
            if gridEqual state.grid slidGrid then
                { grid = slidGrid, score = newScore, won = won, lost = lost, overlayDisplayed = overlay, bestScore = best }

            else
                -- spawn random tile
                { grid = spawnRandomTile slidGrid seed, score = newScore, won = won, lost = lost, overlayDisplayed = overlay, bestScore = best }
    in
    slidGridWithNewTile


slideRight : GameState -> Random.Seed -> GameState
slideRight state seed =
    let
        ( slidGrid_, accScore ) =
            state.grid
                |> rotateGridClockwise
                |> rotateGridClockwise
                |> slideGrid

        newScore =
            state.score + accScore

        slidGrid =
            slidGrid_
                |> rotateGridClockwise
                |> rotateGridClockwise

        won =
            gameWon slidGrid

        lost =
            gameLost slidGrid

        overlay =
            won || lost

        best =
            if newScore > state.bestScore then
                newScore

            else
                state.bestScore

        slidGridWithNewTile =
            -- If grid is same after the slide, dont spawn new tile
            if gridEqual state.grid slidGrid then
                { grid = slidGrid, score = newScore, won = won, lost = lost, overlayDisplayed = overlay, bestScore = best }

            else
                { grid = spawnRandomTile slidGrid seed, score = newScore, won = won, lost = lost, overlayDisplayed = overlay, bestScore = best }
    in
    slidGridWithNewTile


slideUp : GameState -> Random.Seed -> GameState
slideUp state seed =
    let
        ( slidGrid_, accScore ) =
            state.grid
                |> rotateGridClockwise
                |> rotateGridClockwise
                |> rotateGridClockwise
                |> slideGrid

        newScore =
            state.score + accScore

        slidGrid =
            slidGrid_
                |> rotateGridClockwise

        won =
            gameWon slidGrid

        lost =
            gameLost slidGrid

        overlay =
            won || lost

        best =
            if newScore > state.bestScore then
                newScore

            else
                state.bestScore

        slidGridWithNewTile =
            -- If grid is same after the slide, dont spawn new tile
            if gridEqual state.grid slidGrid then
                { grid = slidGrid, score = newScore, won = won, lost = lost, overlayDisplayed = overlay, bestScore = best }

            else
                -- spawn random tile
                { grid = spawnRandomTile slidGrid seed, score = newScore, won = won, lost = lost, overlayDisplayed = overlay, bestScore = best }
    in
    slidGridWithNewTile



--------------------------------------------
-- Methdods for spawning random tiles
--------------------------------------------
-- Define a generator for generating random Int within the range [0, 3]


randIdx : Generator Int
randIdx =
    Random.int 0 3



-- Method to get a random index using the generator


generateRandomIdx : Random.Seed -> ( Int, Random.Seed )
generateRandomIdx seed =
    Random.step randIdx seed



-- Define a weighted generator to give back a 2 or 4


ranTileNum : Random.Generator Int
ranTileNum =
    Random.weighted ( 80, 2 ) [ ( 20, 4 ) ]



-- Generate a 2 or a 4 for spawning random tiles


generateRanTileNum : Random.Seed -> ( Int, Random.Seed )
generateRanTileNum seed =
    Random.step ranTileNum seed



-- Set tile at specified place in a row


setTileInRow : Row -> Int -> Tile -> Row
setTileInRow row cInd newTile =
    -- Go over row
    List.indexedMap
        (\cIdx tile ->
            if cIdx == cInd then
                -- put new tile at index
                newTile

            else
                tile
        )
        row



-- Set tile at specified place in a grid


setTile : Grid -> Int -> Int -> Tile -> Grid
setTile grid rInd cInd newTile =
    -- Go over each row in the grid
    List.indexedMap
        (\rIdx row ->
            if rIdx == rInd then
                -- use helper method to put new tile at place in row
                setTileInRow row cInd newTile

            else
                row
        )
        grid



-- Returns a list of grid coordinates that are the empty tiles in a Grid


getEmptyTilesInRow : Row -> Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
getEmptyTilesInRow row rInd cInd accList =
    case row of
        -- Return at the end
        [] ->
            accList

        tile :: restTiles ->
            if tile == 0 then
                -- Prepend tile coords to list if the tile is empty and s recurse to next col
                getEmptyTilesInRow restTiles rInd (cInd + 1) (( rInd, cInd ) :: accList)

            else
                -- Just recurse to next col in row
                getEmptyTilesInRow restTiles rInd (cInd + 1) accList



-- Helper function to get a list of empty tiles


getEmptyTilesInGrid : Grid -> Int -> List ( Int, Int ) -> List ( Int, Int )
getEmptyTilesInGrid grid_ rInd accumulator =
    case grid_ of
        -- At the end, return the accumulated list
        [] ->
            accumulator

        row :: restRows ->
            -- Get empty tiles in row and recurse to get empty tiles in each row
            getEmptyTilesInGrid restRows (rInd + 1) (getEmptyTilesInRow row rInd 0 accumulator)



-- Spawn a new tile at a random empty tile


spawnRandomTile : Grid -> Random.Seed -> Grid
spawnRandomTile grid seed =
    let
        -- Get a list of empty tiles in the grid
        emptyTiles =
            getEmptyTilesInGrid grid 0 []

        -- Recursive function to generate random indices until finding an empty tile
        generateAndSetTile : Grid -> Random.Seed -> Grid
        generateAndSetTile grid__ seed1 =
            let
                -- generate random row index
                ( randomRow, seed2 ) =
                    generateRandomIdx seed1

                -- random column index
                ( randomCol, seed3 ) =
                    generateRandomIdx seed2

                -- generate random tile num for spawning
                ( twoOrFour, _ ) =
                    generateRanTileNum seed3
            in
            -- If random tile is empty
            if List.member ( randomRow, randomCol ) emptyTiles then
                -- Spawn a tile
                setTile grid__ randomRow randomCol twoOrFour

            else
                -- recurse until empty tile is generated
                generateAndSetTile grid__ seed2
    in
    generateAndSetTile grid seed



--------------------------------------------
-- Methdods for game over/won
--------------------------------------------


tileEqual2048 : Tile -> Bool
tileEqual2048 tile =
    tile == 2048



-- Returns True or False based on whether the given row contains 2048


rowContains2048 : Row -> Bool
rowContains2048 row =
    case row of
        [] ->
            -- If reached the end without finding 2048
            False

        tile :: otherTiles ->
            if tileEqual2048 tile then
                True

            else
                rowContains2048 otherTiles



-- Returns True or False based on whether the given Grid contains 2048


gameWon : Grid -> Bool
gameWon grid =
    case grid of
        [] ->
            -- If reached the end without finding 2048
            False

        row :: otherRows ->
            if rowContains2048 row then
                True

            else
                gameWon otherRows



------ For Game lost detection
-- Check if there's any adjaceents tiles in the row (that aren't empty/0 value)


rowContainsAdjacentTiles : Row -> Bool
rowContainsAdjacentTiles row =
    case row of
        [] ->
            False

        tile1 :: tile2 :: otherTiles ->
            -- If adjacent tile are equal (but are not 0)
            if tile1 == tile2 && tile1 /= 0 && tile2 /= 0 then
                True

            else
                -- recurse on the rest
                rowContainsAdjacentTiles (tile2 :: otherTiles)

        _ :: rest ->
            rowContainsAdjacentTiles rest



-- Check every row in the Grid for emtpy tiles


rowsContainAdjacentTiles : Grid -> Bool
rowsContainAdjacentTiles grid =
    List.any rowContainsAdjacentTiles grid



-- Check if a row contains any empty tiles


rowContainsEmptytiles : Row -> Bool
rowContainsEmptytiles row =
    List.any (\tile -> tile == 0) row



-- Check if a grid contains empty tiles


gridContainsEmptytiles : Grid -> Bool
gridContainsEmptytiles grid =
    List.any rowContainsEmptytiles grid



-- Check if the game is lost


gameLost : Grid -> Bool
gameLost grid =
    let
        -- Check if there are adjacent/equal tiles in rows
        rowsHaveAdjacentEqual =
            rowsContainAdjacentTiles grid

        -- rotate grid to check if cols have adjacent/equal tiles
        rotGrid =
            rotateGridClockwise grid

        colsHaveAdjacentEqual =
            rowsContainAdjacentTiles rotGrid

        -- Check if any empty tiles in the grid
        containsEmptyTiles =
            gridContainsEmptytiles grid

        -- If there's no ajacent tiles that are equal in rows & columns and there's
        -- no empty tiles
        hasNoAdjacentEqualAndNoEmpty =
            not (rowsHaveAdjacentEqual || colsHaveAdjacentEqual || containsEmptyTiles)
    in
    hasNoAdjacentEqualAndNoEmpty

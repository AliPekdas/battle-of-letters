-- Initial game field
gameField :: [(Int, Char)]
gameField = 
    [ (0,'X'), (1,'A'), (2,'-'), (3,'-'), (4,'X'),
      (5,'B'), (6,'-'), (7,'-'), (8,'-'), (9,'Z'),
      (10,'X'), (11,'C'), (12,'-'), (13,'-'), (14,'X') ]

-- Print one row
printRow :: [(Int, Char)] -> IO ()
printRow row = putStrLn (unwords [[c] | (_, c) <- row])

-- Print full game field
printGameField :: [(Int, Char)] -> IO ()
printGameField field = do
    printRow (take 5 field)
    printRow (take 5 (drop 5 field))
    printRow (take 5 (drop 10 field))

-- Safe index lookup for a letter
getLetterIndex :: Char -> [(Int, Char)] -> Maybe Int
getLetterIndex letter field = lookup letter [ (c, i) | (i, c) <- field ]

-- Directions (8 directions)
directions :: [Int]
directions = [-6, -5, -4, -1, 1, 4, 5, 6]

-- Check if move is exactly 1 unit away (adjacent only)
isOneUnitMove :: Int -> Int -> Bool
isOneUnitMove idx delta =
    let row = idx `div` 5
        col = idx `mod` 5
        newIdx = idx + delta
        newRow = newIdx `div` 5
        newCol = newIdx `mod` 5
    in abs (row - newRow) <= 1 && abs (col - newCol) <= 1 && newIdx >= 0 && newIdx < 15

-- Check if move is valid for the given letter to the target cell
isValid :: Char -> Int -> [(Int, Char)] -> Bool
isValid letter target field =
    case getLetterIndex letter field of
        Nothing -> False
        Just currentIndex ->
            let direction = target - currentIndex
                targetChar = snd (field !! target)
                isBackward = direction `elem` [-1, -6, 4]
                isValidDir = direction `elem` directions && isOneUnitMove currentIndex direction
            in target >= 0 && target < 15 && targetChar == '-' && isValidDir && (letter `notElem` "ABC" || not isBackward)

-- Get all valid moves for a letter
validMoves :: Char -> [(Int, Char)] -> [Int]
validMoves letter field =
    case getLetterIndex letter field of
        Nothing -> []
        Just current -> [ target | d <- directions, let target = current + d, target >= 0 && target < 15, isValid letter target field ]

-- Move a letter
moveLetter :: Char -> Int -> [(Int, Char)] -> [(Int, Char)]
moveLetter letter target field =
    case getLetterIndex letter field of
        Nothing -> field
        Just currentIndex -> map update field
          where
            update (i, c)
              | i == target = (i, letter)
              | i == currentIndex = (i, '-')
              | otherwise = (i, c)

-- Main
main :: IO ()
main = do
    putStrLn "Welcome!"
    printGameField gameField
    putStrLn "Enter the maximum number of total moves allowed:"
    maxMovesStr <- getLine
    let maxMoves = read maxMovesStr :: Int
    putStrLn "Who starts first? Type 'last' or 'firsts':"
    start <- getLine
    gameLoop gameField start maxMoves 0 0

-- Game loop
gameLoop :: [(Int, Char)] -> String -> Int -> Int -> Int -> IO ()
gameLoop field start maxMoves currentMoves currentTurn = do
    printGameField field

    let nextTurn = if currentTurn == 0 then 1 else 0
        isFirstsTurn = (start == "firsts" && currentTurn == 0) || (start == "last" && currentTurn == 1)

    if currentMoves >= maxMoves then
        putStrLn "Draw! You've reached the maximum number of moves."
        else do
            if isFirstsTurn then
                putStrLn "Please select one of the first three letters and a cell to move it (e.g., A 6):"
            else
                putStrLn "Please select a cell for the Z:"
            moveInput <- getLine

            let (letter, index) =
                    if isFirstsTurn then
                        let [letterStr, strIndex] = words moveInput
                        in (head letterStr, read strIndex :: Int)
                    else
                        ('Z', read moveInput :: Int)
        
            if isValid letter index field then do
                let updatedField = moveLetter letter index field
                    Just zIndex = getLetterIndex 'Z' updatedField
                    abcIndices = [ getLetterIndex l updatedField | l <- "ABC" ]
                    zCol = zIndex `mod` 5
                    abcCols = [ idx `mod` 5 | Just idx <- abcIndices ]
        
                if all (zCol <) abcCols then do
                    putStrLn "Z wins!"
                    printGameField updatedField
                else
                    let zOptions = validMoves 'Z' updatedField
                    in if null zOptions then do
                        putStrLn "A&B&C win!"
                        printGameField updatedField
                    else
                        gameLoop updatedField start maxMoves (currentMoves + 1) nextTurn
            else do
                putStrLn "invalid move"
                gameLoop field start maxMoves currentMoves nextTurn

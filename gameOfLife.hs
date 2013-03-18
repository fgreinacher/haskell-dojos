{-
Your task is to write a program to calculate the next
generation of Conway's game of life, given any starting
position. You start with a two dimensional grid of cells, 
where each cell is either alive or dead. The grid is finite, 
and no life can exist off the edges. When calculating the 
next generation of the grid, follow these four rules:

1. Any live cell with fewer than two live neighbours dies, 
   as if caused by underpopulation.
2. Any live cell with more than three live neighbours dies, 
   as if by overcrowding.
3. Any live cell with two or three live neighbours lives 
   on to the next generation.
4. Any dead cell with exactly three live neighbours becomes 
   a live cell.
-}

import Data.List

data Grid = Grid {
    rowCount :: Int,
    columnCount :: Int,
    cells :: [Cell]
}

data CellState = Alive | Dead deriving (Eq, Show)

data Cell = Cell {
    row :: Int,
    column :: Int,
    state :: CellState
} deriving (Eq)

instance (Show Cell) where
    show (Cell row column Alive) = "*"
    show (Cell row column Dead) = "."

instance (Show Grid) where
    show = showGrid

showGrid :: Grid -> String
showGrid grid@(Grid rowCount columnCount cells) = showHeader ++ showBody
    where 
        showHeader = "Grid (" ++ show rowCount ++ " rows, " ++ show columnCount ++ " columns)"
        showBody = foldl (\acc x -> acc ++ "\n" ++ showRow x) "" rows
        showRow row = foldl (\acc x -> acc ++ show x ++ " ") "" row
        rows = groupBy (\x y -> row x == row y) cells

cellAt :: Grid -> Int -> Int -> Maybe Cell
cellAt grid row column 
    | isValidPosition grid row column   = Just $ (cells grid) !! indexOfCell grid row column
    | otherwise                         = Nothing

indexOfCell :: Grid -> Int -> Int -> Int
indexOfCell grid row column = (row * (rowCount grid) + column)

neighbors :: Grid -> Int -> Int -> [Maybe Cell]
neighbors grid row column = filter (/= Nothing) [ cellAt grid (row-1) (column-1),
                              cellAt grid (row)   (column-1),
                              cellAt grid (row+1) (column-1),
                              cellAt grid (row-1) (column),
                              cellAt grid (row+1) (column),
                              cellAt grid (row-1) (column+1),
                              cellAt grid (row)   (column+1),
                              cellAt grid (row+1) (column+1) ]

countLivingNeigbors :: Grid -> Cell -> Int
countLivingNeigbors grid cell = 0 --length $ filter (\x -> state x == Alive) $ neighbors grid cell

nextGrid :: Grid -> Grid
nextGrid grid = Grid (rowCount grid) (columnCount grid) nextCells
    where
        nextCells = map (nextCell grid) (cells grid) 

nextCell :: Grid -> Cell -> Cell
nextCell grid cell
    | isAlive && livingNeighbors < 2    = cellInState Dead
    | isAlive && livingNeighbors > 3    = cellInState Dead
    | isDead && livingNeighbors == 3    = cellInState Alive
    | otherwise                         = cellInState $ state cell
    where
        cellInState            = Cell (row cell) (column cell)
        isAlive                = state cell == Alive 
        isDead                 = state cell /= Alive 
        livingNeighbors        = countLivingNeigbors grid cell

isValidPosition :: Grid -> Int -> Int -> Bool
isValidPosition grid row column
    | row < 0 || row >= rowCount grid           = False
    | column < 0 || column >= columnCount grid  = False
    | otherwise                                 = True


makeGrid :: Int -> Int -> Grid
makeGrid rowCount columnCount
     | rowCount <= 0    = error "Row count must be greater than 0."
     | columnCount <= 0 = error "Column count must be greater than 0."
     | otherwise        = Grid rowCount columnCount cells
     where cells = [ Cell row column Alive | row <- [0 .. rowCount - 1] , column <- [0 .. columnCount - 1] ]
     
     

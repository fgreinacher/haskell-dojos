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

module GameOfLife ( Grid, 
                    CellState, 
                    Cell, 
                    createGrid, 
                    propagateGrid, 
                    animateCell, 
                    killCell) where

import Data.List
import Data.Maybe

data Grid = Grid {
    rowCount :: Int,
    columnCount :: Int,
    cells :: [Cell]
}

instance (Show Grid) where
    show = showGrid

data CellState = Alive | Dead deriving (Eq, Show)

data Cell = Cell {
    row :: Int,
    column :: Int,
    state :: CellState
} deriving (Eq)

instance (Show Cell) where
    show (Cell row column Alive) = "*"
    show (Cell row column Dead) = "-"

-- Creates a grid with the specified number of rows and columns.
createGrid :: Int -> Int -> Grid
createGrid rowCount columnCount
    | rowCount <= 0    = error "Row count must be greater than 0."
    | columnCount <= 0 = error "Column count must be greater than 0."
    | otherwise        = Grid rowCount columnCount cells
    where cells = [ Cell row column Dead | row <- [0 .. rowCount - 1] , column <- [0 .. columnCount - 1] ]
     
-- Propagates the specified grid to the next generation.
propagateGrid :: Grid -> Grid
propagateGrid grid = Grid (rowCount grid) (columnCount grid) propagatedCells
    where
        propagatedCells = map (propagateCell grid) (cells grid) 

-- Brings to specified cell to live.
animateCell :: Grid -> Int -> Int -> Grid
animateCell = setCellState Alive

-- Kills the specified cell.
killCell :: Grid -> Int -> Int -> Grid
killCell = setCellState Dead

-- Internal implementation:

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

isValidPosition :: Grid -> Int -> Int -> Bool
isValidPosition grid row column
    | row < 0 || row >= rowCount grid           = False
    | column < 0 || column >= columnCount grid  = False
    | otherwise                                 = True
    
indexOfCell :: Grid -> Int -> Int -> Int
indexOfCell grid row column = (row * (rowCount grid) + column)

neighbors :: Grid -> Int -> Int -> [Cell]
neighbors grid row column = [fromJust n | n <- potentialNeighbors, n /= Nothing]
    where potentialNeighbors = [ cellAt grid (row-1) (column-1), cellAt grid (row)   (column-1), cellAt grid (row+1) (column-1),
                                 cellAt grid (row-1) (column), cellAt grid (row+1) (column),
                                 cellAt grid (row-1) (column+1), cellAt grid (row)   (column+1), cellAt grid (row+1) (column+1) ]

countLivingNeigbors :: Grid -> Cell -> Int
countLivingNeigbors grid (Cell row column _) = length $ filter (\x -> state x == Alive) $ neighbors grid row column

propagateCell :: Grid -> Cell -> Cell
propagateCell grid cell
    | isAlive && livingNeighbors < 2    = cellInState Dead
    | isAlive && livingNeighbors > 3    = cellInState Dead
    | isDead && livingNeighbors == 3    = cellInState Alive
    | otherwise                         = cellInState $ state cell
    where
        cellInState            = Cell (row cell) (column cell)
        isAlive                = state cell == Alive 
        isDead                 = state cell /= Alive 
        livingNeighbors        = countLivingNeigbors grid cell
          
setCellState :: CellState -> Grid -> Int -> Int -> Grid
setCellState state grid rowOfInterest columnOfInterest = Grid (rowCount grid) (columnCount grid) newCells
    where newCells              = map (\x -> if isCellOfInterest x then newCellFrom x else x) (cells grid)
          isCellOfInterest cell = (row cell) == rowOfInterest && (column cell) == columnOfInterest         
          newCellFrom cell      = (Cell (row cell) (column cell) state)

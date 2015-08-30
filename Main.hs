import Data.Vector (Vector, generate, slice, toList, fromList, (!), (//))
import Data.Either.Unwrap
import Data.Maybe
import Data.List
import System.IO

data Player = X | O deriving (Eq, Show, Enum)
type Tile = Either Int Player
type Board = Vector Tile
data State = State { board :: Board, player :: Player } deriving (Eq, Show)

initialState :: State
initialState = State (generate 9 (\i -> Left i)) X

nextStates :: State -> [State]
nextStates s = filter (\x -> board x /= board s) $ map (flip makeMove s) [0..8]

makeMove :: Int -> State -> State
makeMove i s = State newBoard (nextPlayer $ player s)
    where 
        t = board s ! i
        newBoard
            | isLeft t = board s // [(i, Right $ player s)]
            | otherwise = board s

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

negamax :: Int -> Player -> State -> Int
negamax d p s
        | winnerOf s == Just p = 100
        | winnerOf s == Just (nextPlayer p) = -100
        | d == 0 || (all isRight $ board s) = 0
        | otherwise = minimum $
            map (negate . negamax (d - 1) (nextPlayer p)) (nextStates s)

winnerOf :: State -> Maybe Player
winnerOf s = maybe Nothing id $
        find isJust $ map winner rows ++ map winner cols ++ map winner diags
    where
        winner [a, b, c] = 
            if all isRight [a, b, c] && a == b && b == c
            then Just (fromRight a) else Nothing
        rows  = map (\i -> toList $ slice (3*i) 3 (board s)) [0..2]
        cols  = map (\i -> map ((!) $ board s) [i, i+3, i+6]) [0..2]
        diags = map (map ((!) (board s))) [[0, 4, 8], [2, 4, 6]]

-- temporary tests to see if shit works
main = do
    print $ map (negamax 10 X) $ nextStates
        $ State (fromList [Right X, Right O, Left 2,
                           Left 3, Left 4, Left 5,
                           Left 6, Left 7, Left 8]) X

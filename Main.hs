import Data.Vector (Vector, generate, slice, toList, (!), (//))
import Data.Either
import Data.Maybe
import Debug.Trace

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

isGameOver :: State -> Bool
isGameOver s = any (==True) $ map same rows ++ map same cols ++ map same diags
    where
        same [a, b, c] = all isRight [a, b, c] && a == b && b == c
        rows  = map (\i -> toList $ slice (3*i) 3 (board s)) [0..2]
        cols  = map (\i -> map ((!) $ board s) [i, i+3, i+6]) [0..2]
        diags = map (map ((!) (board s))) [[0, 4, 8], [2, 4, 6]]

-- temporary tests to see if shit works
main = do
    print initialState
    print $ makeMove 0 $ initialState
    --print $ isGameOver $ makeMove 0 $ initialState
    --let row = foldl (flip (.)) id (map makeMove [0, 3, 1, 4, 2]) $ initialState
    --print $ row
    --print $ isGameOver row
    print $ take 2 . toList . board $ makeMove 6 initialState
    print $ take 2 . toList . board $ makeMove 6 initialState
    print $ take 5 $ nextStates initialState
    print $ take 5 $ nextStates initialState

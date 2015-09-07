import qualified Data.Vector as V
import Data.Vector ((//), (!))
import Data.Maybe
import Data.List
import Data.Tree

data Player = Black | White deriving (Eq, Show, Enum)
type Tile = Maybe Player
type Board = V.Vector Tile
data State = State { board :: Board, player :: Player } deriving (Eq, Show)

prettyPrint :: Board -> String
prettyPrint b
    | not (null rows) = (V.toList $ V.map (maybe '.' color) row)
                         ++ "\n" ++ prettyPrint rows
    | otherwise = ""
    where
      (row, rows) = V.splitAt 8 b
      color p = if p == Black then '○' else '●'

initialState :: State
initialState = State ((V.replicate (8 * 8) Nothing)
                // [(to1D (3, 3), Just White), (to1D (4, 4), Just White),
                    (to1D (3, 4), Just Black), (to1D (4, 3), Just Black)]) Black

nextStates :: State -> [State]
nextStates s = mapMaybe (makeMove s) [0 .. 8 * 8 - 1]

makeMove :: State -> Int -> Maybe State
makeMove s i
    | isJust $ (board s) ! i = Nothing
    | null flips = Nothing
    | otherwise = Just $ State
                (( foldl' (flipTiles (player s)) (board s) flips)
                // [(i, Just $ player s)]) (nextPlayer $ player s)
    where
      flips = filter ((> 0) . length)
            $ map (gatherOpposing s (to2D i)) directions

gameTree :: Tree State
gameTree = unfoldTree gameTreeGen initialState
gameTreeGen :: State -> (State, [State])
gameTreeGen s = (s, nextStates s)

nextPlayer :: Player -> Player
nextPlayer Black = White
nextPlayer White = Black

flipTiles :: Player -> Board -> [Int] -> Board
flipTiles p b ix = b // (map  (\i -> (i, Just p)) ix)

gatherOpposing :: State -> (Int, Int) -> (Int, Int) -> [Int]
gatherOpposing s p dp  = gatherOpposingAcc s p dp []

gatherOpposingAcc :: State -> (Int, Int) -> (Int, Int) -> [Int] -> [Int]
gatherOpposingAcc s (x, y) (dx, dy) acc
    | not (inBounds (x + dx, y + dy)) = []
    | isFlippable = gatherOpposingAcc s dp (dx, dy) (to1D dp:acc)
    | isEnd && not (null acc) = acc
    | otherwise = []
    where
        isFlippable = maybe False (/= (player s)) t
        isEnd = maybe False (== (player s)) t
        t = board s ! to1D dp
        dp = (x + dx, y + dy)

inBounds :: (Int, Int) -> Bool
inBounds (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

directions :: [(Int, Int)]
directions = [(x,y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]

to2D :: Int -> (Int, Int)
to2D i = (i `mod` 8, i `div` 8)

to1D :: (Int, Int) -> Int
to1D (x, y) = x + y * 8

-- temporary tests to see if shit works
main = do
    print $ (length . concat . (take 9) . levels) gameTree
    --mapM_ putStrLn $ map (prettyPrint . board) $ concat $ take 3 $ levels gameTree

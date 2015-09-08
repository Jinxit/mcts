import qualified Data.Vector as V
import Data.Vector ((//), (!))
import Data.Maybe
import Data.List

data Player
    = Black
    | White
    deriving (Eq, Show, Enum, Bounded)
type Tile = Maybe Player
type Board = V.Vector Tile
data State = State
    { board :: Board
    , player :: Player
    } deriving (Eq, Show)

-- IMPLEMENTED BY GAME MODULE

nextPlayer :: (Enum a, Bounded a) => a -> a
nextPlayer p = toEnum (add (fromEnum (maxBound `asTypeOf` p) + 1) (fromEnum p) 1)
  where
    add mod x y = (x + y + mod) `rem` mod

negamax :: (a -> Int) -> (a -> [a]) -> Int -> a -> Int
negamax scoring moveGen d s
    | null moves || d == 0 = scoring s
    | otherwise = minimum negaMoves
  where
    moves = moveGen s
    negaMoves = map (negate . negamax scoring moveGen (d - 1)) moves

-- REVERSI

prettyPrint :: Board -> String
prettyPrint b
    | not (null row) = (V.toList $ V.map (maybe '.' color) row)
                        ++ "\n" ++ prettyPrint rows
    | otherwise = ""
  where
    (row, rows) = V.splitAt 8 b
    color p = if p == Black then '○' else '●'

maxBranches :: Int
maxBranches = 64

initialState :: State
initialState = State ((V.replicate (8 * 8) Nothing)
                  // [(to1D (3, 3), Just White), (to1D (4, 4), Just White),
                      (to1D (3, 4), Just Black), (to1D (4, 3), Just Black)])
                     Black

nextStates :: State -> [State]
nextStates s = mapMaybe (makeMove s) [0 .. maxBranches - 1]

makeMove :: State -> Int -> Maybe State
makeMove s i
    | isJust $ (board s) ! i = Nothing
    | null flips = Nothing
    | otherwise = Just $ State
                ((foldl' (flipTiles (player s)) (board s) flips)
              // [(i, Just $ player s)])
                (nextPlayer $ player s)
  where
    flips = filter (not . null)
          $ map (gatherOpposing s (to2D i)) directions

score :: State -> Int
score s = sum $ map (\t -> if t == (player s) then 1 else -1) stones
  where
    stones = catMaybes $ V.toList $ board s

flipTiles :: Player -> Board -> [Int] -> Board
flipTiles p b ix = b // (map  (\i -> (i, Just p)) ix)

gatherOpposing :: State -> (Int, Int) -> (Int, Int) -> [Int]
gatherOpposing s p dp = gatherOpposingAcc s p dp []

gatherOpposingAcc :: State -> (Int, Int) -> (Int, Int) -> [Int] -> [Int]
gatherOpposingAcc s (x, y) (dx, dy) acc
    | not (inBounds (x + dx, y + dy)) = []
    | isFlippable = gatherOpposingAcc s dp (dx, dy) (to1D dp:acc)
    | isEnd = acc
    | otherwise = []
  where
    isFlippable = maybe False (/= (player s)) t
    isEnd = maybe False (== (player s)) t
    t = board s ! to1D dp
    dp = (x + dx, y + dy)

directions :: [(Int, Int)]
directions = [(x,y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]

inBounds :: (Int, Int) -> Bool
inBounds (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

to2D :: Int -> (Int, Int)
to2D i = (i `mod` 8, i `div` 8)

to1D :: (Int, Int) -> Int
to1D (x, y) = x + y * 8

-- temporary tests to see if shit works
main = do
    print $ map (negamax score nextStates 4) $ nextStates initialState
    putStrLn $ prettyPrint $ board (nextStates initialState !! 1)

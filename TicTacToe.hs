import qualified Data.Vector as V
import Data.Vector ((//), (!))
import Data.Maybe
import Data.List

data Player
    = X
    | O
    deriving (Eq, Show, Enum, Bounded)
type Tile = Maybe Player
type Board = V.Vector Tile
data State = State
   { board :: Board
   , player :: Player
   } deriving (Eq, Show)

-- IMPLEMENTED BY INTERFACE

nextPlayer :: Player -> Player
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

-- TIC-TAC-TOE

prettyPrint :: Board -> String
prettyPrint b
    | not (null row) = (V.toList $ V.map (maybe '.' color) row)
                        ++ "\n" ++ prettyPrint rows
    | otherwise = ""
  where
    (row, rows) = V.splitAt 3 b
    color p = if p == O then '○' else '✕'

maxBranches :: Int
maxBranches = 9

initialState :: State
initialState = State (V.replicate maxBranches Nothing) X

nextStates :: State -> [State]
nextStates s = mapMaybe (makeMove s) [0 .. maxBranches - 1]

makeMove :: State -> Int -> Maybe State
makeMove s i
    | isJust $ (board s) ! i = Nothing
    | otherwise = Just $ State
                         (board s // [(i, Just $ player s)])
                         (nextPlayer $ player s)

score :: State -> Int
score s = maybe 0 (\p -> if p /= player s then 100 else -100) (winnerOf s)

winnerOf :: State -> Maybe Player
winnerOf s = fromMaybe Nothing $
        find isJust $ map winner $ map (map ((!) (board s))) winLines
    where
        winner [a, b, c] = 
            if all isJust [a, b, c] && a == b && b == c
            then Just (fromJust a) else Nothing

winLines :: [[Int]]
winLines = rows ++ cols ++ diags
  where
    rows  = [[i..i+2] | i <- [0, 3, 6]]
    cols  = [[i, i + 3, i + 6] | i <- [0..2]]
    diags = [[0, 4, 8], [2, 4, 6]]

-- temporary tests to see if shit works
main = do
    let s = State (V.fromList [ Just X,  Just O, Nothing,
                                Just O,  Just X, Nothing,
                               Nothing, Nothing, Nothing]) X
    mapM_ putStrLn $ map (prettyPrint . board) $ nextStates initialState
    print $ map (negamax score nextStates 7) $ nextStates initialState

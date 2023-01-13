module Year2016.Day01 where

import Prelude (Unit, bind, discard, show, ($), (<$>), negate)

import Effect (Effect)
import Effect.Console (log)
import Data.String as String
import Data.Int as Int

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String.Pattern (Pattern(..))
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.List (fromFoldable, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Semiring ((+), (*))
import Data.Traversable (sequence)
import Data.Ord (abs, (<=))
import Data.Foldable (foldl)
import Data.Show (class Show)
import Control.Bind ((>>=))
import Data.Set as Set
import Data.Semigroup ((<>))
import Data.List (singleton) as List
import Data.Ring ((-))
import Data.List.Types

data Direction
  = North
  | East
  | South
  | West

instance showDirection :: Show Direction where
  show North = "N"
  show East = "E"
  show South = "S"
  show West = "W"

data Rotation
  = Left90
  | Right90
  | NoRotation

instance showRotation :: Show Rotation where
  show Left90 = "L"
  show Right90 = "R"
  show NoRotation = "_"

type Command =
  { rotation :: Rotation
  , steps :: Int
  }

type Coords2D =
  { x :: Int
  , y :: Int
  }

origin :: Coords2D
origin =
  { x: 0
  , y: 0
  }

type State =
  { coords :: Coords2D
  , direction :: Direction
  }

stringToListOfChars :: String -> List Char
stringToListOfChars s = fromFoldable $ toCharArray s

charArrayToString :: List Char -> String
charArrayToString a = fromCharArray (toUnfoldable a)

charListToInt :: List Char -> Maybe Int
charListToInt s = Int.fromString (charArrayToString s)

parseRotation :: Char -> Maybe Rotation
parseRotation 'L' = Just Left90
parseRotation 'R' = Just Right90
parseRotation _ = Nothing

parseListCharToCommand :: List Char -> Maybe Command
parseListCharToCommand (r : s) = do
  rotation <- parseRotation r
  steps <- charListToInt s
  Just { rotation: rotation, steps: steps }
parseListCharToCommand _ = Nothing

parseCommand :: String -> Maybe Command
parseCommand s = parseListCharToCommand $ stringToListOfChars s

parse :: String -> Maybe (Array Command)
parse s = sequence $ parseCommand <$> segments
  where
    segments :: Array String
    segments = String.split (Pattern ", ") s

start :: State
start = { coords: origin, direction: North }

rotate :: Direction -> Rotation -> Direction
rotate North Right90 = East
rotate East Right90 = South
rotate South Right90 = West
rotate West Right90 = North
rotate North Left90 = West
rotate East Left90 = North
rotate South Left90 = East
rotate West Left90 = South
rotate r NoRotation = r

delta :: Direction -> Coords2D
delta North = { x : 0, y : -1 }
delta East = { x : 1, y : 0 }
delta South = { x : 0, y : 1 }
delta West = { x : -1, y : 0 }

applyCommand :: State -> Command -> State
applyCommand state command = applyMovement (applyRotation state command.rotation) command.steps
  where
    applyMovement :: State -> Int -> State
    applyMovement s movement = { coords: s.coords + (mult (delta s.direction) movement), direction: s.direction }

    applyRotation :: State -> Rotation -> State
    applyRotation s rotation = { coords: s.coords, direction: rotate s.direction rotation }

    mult :: Coords2D -> Int -> Coords2D
    mult coords n = { x: coords.x * n, y : coords.y * n }

endPoint :: Array Command -> Coords2D
endPoint commands = endState.coords
  where
    endState = foldl applyCommand start commands

firstIntersection :: Array Command -> Maybe Coords2D
firstIntersection commands = f start (expand $ fromFoldable commands) Set.empty
  where
    f :: State -> List Command -> Set.Set Coords2D -> Maybe Coords2D
    f state remaining visited =
      if Set.member state.coords visited
      then Just state.coords
      else
        case remaining of
          (x : xs) -> f (applyCommand state x) xs (Set.insert state.coords visited)
          _ -> Nothing

    expand :: List Command -> List Command
    expand (x : xs) = expandCommand x <> expand xs
      where
        expandCommand :: Command -> List Command
        expandCommand c =
          if c.steps <= 1
          then List.singleton c
          else Cons { rotation: c.rotation, steps: 1 } (expandCommand { rotation: NoRotation, steps: c.steps - 1 })
    expand _ = Nil

manhattanDistanceToOrigin :: Coords2D -> Int
manhattanDistanceToOrigin c = abs c.x + abs c.y

part1 :: String -> Maybe Int
part1 s = solve1 <$> parse s
  where
    solve1 :: Array Command -> Int
    solve1 commands = manhattanDistanceToOrigin $ endPoint commands

part2 :: String -> Maybe Int
part2 s = parse s >>= solve2
  where
    solve2 :: Array Command -> Maybe Int
    solve2 commands = manhattanDistanceToOrigin <$> firstIntersection commands

readRealInput :: Effect String
readRealInput = readTextFile UTF8 "./resources/Year2016/Day01.txt"

main :: Effect Unit
main = do
  input <- readRealInput
  log $ show $ part1 input
  log $ show $ part2 input

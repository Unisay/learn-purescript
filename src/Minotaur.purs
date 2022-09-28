module Minotaur where

import Prelude
import Prim hiding (Row)

import Control.Lazy (fix)
import Control.MonadPlus (guard)
import Data.Array ((!!))
import Data.Array as A
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum, pred, succ, upFromIncluding)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Int (rem)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Natural (Natural, intToNat, natToInt)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Random.LCG (lcgNext)
import Random.PseudoRandom (class Random, Seed, mkSeed, random, randomRs)

type Row = Array Loc

data Maze = Maze { rows ∷ Array Row }

instance showMaze ∷ Show Maze where
  show = renderMaze

data Loc
  = Free
  | SoftWall
  | HardWall

derive instance genericLoc ∷ Generic Loc _

instance eqLoc ∷ Eq Loc where
  eq = genericEq

instance showLoc ∷ Show Loc where
  show = genericShow

type Size = { width ∷ Natural, height ∷ Natural }

type Pos = { x ∷ Natural, y ∷ Natural }

data Direction
  = North
  | East
  | South
  | West

derive instance genericDirection ∷ Generic Direction _

instance eqDirection ∷ Eq Direction where
  eq = genericEq

instance ordDirection ∷ Ord Direction where
  compare = genericCompare

instance boundedDirection ∷ Bounded Direction where
  top = genericTop
  bottom = genericBottom

instance enumDirection ∷ Enum Direction where
  succ = genericSucc
  pred = genericPred

instance showDirection ∷ Show Direction where
  show = genericShow

instance randomDirection ∷ Random Direction where
  random seed =
    let
      { newSeed, newVal } = random seed
    in
      case rem newVal 4 of
        0 → { newSeed, newVal: North }
        1 → { newSeed, newVal: East }
        2 → { newSeed, newVal: South }
        3 → { newSeed, newVal: West }
        _ → unsafeCrashWith "Impossible: invalid random direction"

type State =
  { seed ∷ Seed
  , pos ∷ Pos
  , way ∷ Array Pos
  , maze ∷ Maze
  , noWay ∷ Boolean
  }

initialPos ∷ Pos
initialPos = { x: intToNat 13, y: zero }

initialState ∷ State
initialState =
  { pos: initialPos
  , way: []
  , seed: mkSeed 45
  , maze: brickMaze { width: intToNat 51, height: intToNat 22 }
  , noWay: false
  }

renderMaze ∷ Maze → String
renderMaze (Maze { rows }) = A.foldMap (append "\n") renderedRows
  where
  renderedRows ∷ Array String
  renderedRows = map renderRow (A.reverse rows)

  renderRow ∷ Row → String
  renderRow =
    A.foldMap case _ of
      Free → "  "
      SoftWall → "▓▓"
      HardWall → "██"

carvedMaze ∷ State → Maze
carvedMaze =
  _.maze
    <<< fix \next st → case carve st of
      { noWay: true, way: [] } → st
      { noWay: true, way }
        | Just { head, tail } ← A.uncons way → do
            next $ st { pos = head, way = tail }
      s → next s

carve ∷ State → State
carve state =
  fromMaybe (state { noWay = true })
    $ A.find (not <<< _.noWay)
    $ randomDirections state.seed
        <#> \direction → breakBricksInDirection direction state

breakBricksInDirection ∷ Direction → State → State
breakBricksInDirection direction state =
  fromMaybe (state { noWay = true }) do
    pos' ← move direction state.pos
    maze' ← breakBrick pos' state.maze
    pos'' ← move direction pos'
    maze'' ← breakBrick pos'' maze'
    pure
      $ state
          { seed = lcgNext state.seed
          , pos = pos''
          , way = A.cons state.pos state.way
          , maze = maze''
          }

move ∷ Direction → Pos → Maybe Pos
move dir pos = case dir of
  North → succ pos.y <#> pos { y = _ }
  East → succ pos.x <#> pos { x = _ }
  South → pred pos.y <#> pos { y = _ }
  West → pred pos.x <#> pos { x = _ }

breakBrick ∷ Pos → Maze → Maybe Maze
breakBrick { x, y } (Maze maze) = do
  loc ← do
    row ← maze.rows !! natToInt y
    row !! natToInt x
  guard $ loc `eq` SoftWall
  rows' ←
    A.alterAt (natToInt y)
      ( \row →
          Just
            $ fromMaybe row
            $ A.updateAt (natToInt x) Free row
      )
      maze.rows
  pure $ Maze { rows: rows' }

randomDirections ∷ ∀ a. Enum a ⇒ Bounded a ⇒ Seed → Array a
randomDirections seed = A.foldl swap vals swaps
  where
  vals ∷ Array a
  vals = upFromIncluding bottom

  swaps ∷ Array Int
  swaps = randomRs 0 3 (A.length vals) seed

  swap ∷ Array a → Int → Array a
  swap xs i = unsafePartial $ fromJust $ A.insertAt i head tail
    where
    { head, tail } = unsafePartial $ fromJust $ A.uncons xs

brickMaze ∷ Size → Maze
brickMaze size =
  Maze
    { rows:
        join
          [ [ hardWall ]
          , A.replicate (natToInt size.height - 2) softWall
          , [ hardWall ]
          ]
    }
  where
  softWall ∷ Array Loc
  softWall =
    join
      [ [ HardWall ]
      , A.replicate (natToInt size.width - 2) SoftWall
      , [ HardWall ]
      ]

  hardWall ∷ Array Loc
  hardWall = A.replicate (natToInt size.width) HardWall

/-
  Blockfall.Game.State
  Game state structure
-/
import Blockfall.Core
import Blockfall.Game.Random
import Blockfall.Game.Scoring

namespace Blockfall.Game

open Blockfall.Core

/-- Complete game state -/
structure GameState where
  board : Board
  current : Piece
  currentPos : Point
  next : PieceType
  bag : Bag
  score : Nat
  level : Nat
  linesCleared : Nat
  tickCounter : Nat
  gameOver : Bool
  paused : Bool
  deriving Repr, Inhabited

/-- Create initial game state -/
def GameState.new (seed : UInt64) : GameState := Id.run do
  let rng := RNG.new seed
  let mut bag := Bag.new rng
  let (firstType, bag') := bag.next
  bag := bag'
  let nextType := bag.peek
  {
    board := Board.empty
    current := Piece.fromType firstType
    currentPos := spawnPos
    next := nextType
    bag := bag
    score := 0
    level := 1
    linesCleared := 0
    tickCounter := 0
    gameOver := false
    paused := false
  }

/-- Spawn a new piece -/
def GameState.spawnPiece (state : GameState) : GameState :=
  let (nextType, newBag) := state.bag.next
  let newPiece := Piece.fromType state.next
  let over := isGameOver state.board newPiece
  { state with
    current := newPiece
    currentPos := spawnPos
    next := nextType
    bag := newBag
    gameOver := over
  }

/-- Get the ghost piece Y position -/
def GameState.ghostY (state : GameState) : Int :=
  Blockfall.Core.ghostY state.board state.current state.currentPos

/-- Current gravity delay based on level -/
def GameState.gravityDelay (state : GameState) : Nat :=
  Blockfall.Game.gravityDelay state.level

end Blockfall.Game

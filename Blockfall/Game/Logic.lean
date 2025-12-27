/-
  Blockfall.Game.Logic
  Game logic functions
-/
import Blockfall.Core
import Blockfall.Game.State
import Blockfall.Game.Scoring

namespace Blockfall.Game

open Blockfall.Core

/-- Move the current piece in a direction -/
def move (state : GameState) (dir : Direction) : GameState :=
  let newPos := state.currentPos + dir.toOffset
  if collides state.board state.current newPos then
    state
  else
    { state with currentPos := newPos }

/-- Move left -/
def moveLeft (state : GameState) : GameState :=
  move state .left

/-- Move right -/
def moveRight (state : GameState) : GameState :=
  move state .right

/-- Soft drop (move down one) and award points -/
def softDrop (state : GameState) : GameState :=
  let newPos := state.currentPos + Direction.down.toOffset
  if collides state.board state.current newPos then
    state
  else
    { state with
      currentPos := newPos
      score := state.score + softDropPoints
    }

/-- Rotate the current piece clockwise with wall kicks -/
def rotate (state : GameState) : GameState :=
  match tryRotate state.board state.current state.currentPos true with
  | some (newPiece, newPos) =>
    { state with current := newPiece, currentPos := newPos }
  | none => state

/-- Lock the current piece and handle line clears -/
def lock (state : GameState) : GameState := Id.run do
  -- Lock piece to board
  let board := lockPiece state.board state.current state.currentPos
  -- Clear lines
  let (clearedBoard, numCleared) := board.clearLines
  -- Update score and level
  let newLinesCleared := state.linesCleared + numCleared
  let newLevel := levelFromLines newLinesCleared
  let points := linePoints numCleared newLevel
  -- Spawn new piece
  let mut newState := { state with
    board := clearedBoard
    score := state.score + points
    linesCleared := newLinesCleared
    level := newLevel
    tickCounter := 0
  }
  newState := newState.spawnPiece
  newState

/-- Hard drop - drop instantly and lock -/
def hardDrop (state : GameState) : GameState :=
  let targetY := state.ghostY
  let dropDistance := (targetY - state.currentPos.y).toNat
  let points := dropDistance * hardDropPoints
  let droppedState := { state with
    currentPos := ⟨state.currentPos.x, targetY⟩
    score := state.score + points
  }
  lock droppedState

/-- Apply gravity (called each frame) -/
def applyGravity (state : GameState) : GameState :=
  if state.paused || state.gameOver then state
  else
    let newTick := state.tickCounter + 1
    if newTick >= state.gravityDelay then
      -- Time to drop
      let newPos := state.currentPos + Direction.down.toOffset
      if collides state.board state.current newPos then
        -- Can't move down, lock piece
        lock state
      else
        { state with currentPos := newPos, tickCounter := 0 }
    else
      { state with tickCounter := newTick }

/-- Toggle pause state -/
def togglePause (state : GameState) : GameState :=
  { state with paused := !state.paused }

/-- Restart the game -/
def restart (_state : GameState) (seed : UInt64) : GameState :=
  GameState.new seed

end Blockfall.Game

/-
  Blockfall Tests
-/
import Crucible
import Blockfall

namespace Blockfall.Tests

open Crucible
open Blockfall.Core
open Blockfall.Game

testSuite "Blockfall Tests"

-- Core Types Tests

test "Point addition" := do
  let p1 : Point := ⟨1, 2⟩
  let p2 : Point := ⟨3, 4⟩
  let sum := p1 + p2
  sum.x ≡ 4
  sum.y ≡ 6

test "Direction offsets" := do
  Direction.left.toOffset.x ≡ -1
  Direction.right.toOffset.x ≡ 1
  Direction.down.toOffset.y ≡ 1

-- Piece Tests

test "All piece types defined" := do
  PieceType.all.length ≡ 7

test "Piece cells count" := do
  -- Each piece has exactly 4 cells
  for t in PieceType.all do
    let cells := t.cells 0
    ensure (cells.length == 4) s!"Piece {repr t} should have 4 cells"

test "O piece doesn't change on rotation" := do
  let cells0 := PieceType.O.cells 0
  let cells1 := PieceType.O.cells 1
  let cells2 := PieceType.O.cells 2
  let cells3 := PieceType.O.cells 3
  ensure (cells0 == cells1) "O piece rotation 0 should equal 1"
  ensure (cells1 == cells2) "O piece rotation 1 should equal 2"
  ensure (cells2 == cells3) "O piece rotation 2 should equal 3"

test "Piece rotation cycles" := do
  let p := Piece.fromType .T
  let p1 := p.rotateCW
  let p2 := p1.rotateCW
  let p3 := p2.rotateCW
  let p4 := p3.rotateCW
  p4.rotation ≡ 0  -- Should cycle back

-- Board Tests

test "Empty board" := do
  let board := Board.empty
  for x in List.range boardWidth do
    for y in List.range boardHeight do
      ensure (board.get x y == none) s!"Cell ({x}, {y}) should be empty"

test "Board set and get" := do
  let board := Board.empty
  let board' := board.set 5 10 (some .red)
  board'.get 5 10 ≡ some .red
  board'.get 0 0 ≡ none

test "Row completion detection" := do
  let mut board := Board.empty
  -- Fill one row
  for x in List.range boardWidth do
    board := board.set x 19 (some .blue)
  ensure (board.isRowComplete 19) "Row 19 should be complete"
  ensure (!board.isRowComplete 0) "Row 0 should not be complete"

test "Line clearing" := do
  let mut board := Board.empty
  -- Fill bottom row
  for x in List.range boardWidth do
    board := board.set x 19 (some .cyan)
  -- Add one cell above
  board := board.set 5 18 (some .red)

  let (cleared, count) := board.clearLines
  count ≡ 1
  -- The cell above should have shifted down
  cleared.get 5 19 ≡ some .red

-- Collision Tests

test "Collision with floor" := do
  let board := Board.empty
  let piece := Piece.fromType .O
  let pos : Point := ⟨4, 19⟩  -- At bottom
  ensure (collides board piece pos) "Piece at bottom should collide"

test "Collision with walls" := do
  let board := Board.empty
  let piece := Piece.fromType .I
  let posLeft : Point := ⟨-1, 5⟩
  let posRight : Point := ⟨8, 5⟩
  ensure (collides board piece posLeft) "Piece past left wall should collide"
  ensure (collides board piece posRight) "Piece past right wall should collide"

test "No collision in valid position" := do
  let board := Board.empty
  let piece := Piece.fromType .T
  let pos := spawnPos
  ensure (!collides board piece pos) "Piece at spawn should not collide"

-- Ghost piece test

test "Ghost Y calculation" := do
  let board := Board.empty
  let piece := Piece.fromType .O
  let pos := spawnPos
  let ghost := ghostY board piece pos
  -- Ghost should be near the bottom
  ensure (ghost > 15) "Ghost should be near bottom on empty board"

-- Scoring Tests

test "Line scoring" := do
  linePoints 1 1 ≡ 100   -- Single at level 1
  linePoints 4 1 ≡ 800   -- Tetris at level 1
  linePoints 2 5 ≡ 1500  -- Double at level 5

test "Level calculation" := do
  levelFromLines 0 ≡ 1
  levelFromLines 9 ≡ 1
  levelFromLines 10 ≡ 2
  levelFromLines 25 ≡ 3

-- Game State Tests

test "Initial state" := do
  let state := GameState.new 12345
  state.score ≡ 0
  state.level ≡ 1
  state.linesCleared ≡ 0
  ensure (!state.gameOver) "Game should not be over initially"
  ensure (!state.paused) "Game should not be paused initially"

test "Movement" := do
  let state := GameState.new 12345
  let startX := state.currentPos.x
  let movedLeft := moveLeft state
  let movedRight := moveRight state
  movedLeft.currentPos.x ≡ startX - 1
  movedRight.currentPos.x ≡ startX + 1

test "Pause toggle" := do
  let state := GameState.new 12345
  ensure (!state.paused) "Should start unpaused"
  let paused := togglePause state
  ensure paused.paused "Should be paused after toggle"
  let unpaused := togglePause paused
  ensure (!unpaused.paused) "Should be unpaused after second toggle"

#generate_tests

end Blockfall.Tests

def main : IO UInt32 := do
  IO.println "╔════════════════════════════════════════╗"
  IO.println "║        Blockfall Test Suite            ║"
  IO.println "╚════════════════════════════════════════╝"
  IO.println ""

  let result ← runAllSuites

  IO.println ""
  if result == 0 then
    IO.println "✓ All tests passed!"
  else
    IO.println "✗ Some tests failed"

  return result

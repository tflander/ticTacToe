package ticTacToe

object CellState extends Enumeration {
  type CellState = Value
  val Clear, X, O = Value
}

import CellState._

class Board(cells: Array[Array[CellState]]) {

  val boardSize = 3
  val boardSizeMinusOne = boardSize - 1

  def cellState(col: Int, row: Int): CellState = {
    return cells(col)(row)
  }

  def setCellState(square: (Int, Int), cellState: CellState): Board = {
    setCellState(square._1, square._2, cellState)
  }

  def setCellState(col: Int, row: Int, cellState: CellState): Board = {
    require(col < boardSize && row < boardSize)

    val newState =
      for (c <- 0 to boardSizeMinusOne)
        yield for (r <- 0 to boardSizeMinusOne)
        yield if (c == col && r == row) cellState else cells(c)(r)

    val updatedCells = newState.map(_.toArray).toArray

    return new Board(updatedCells)
  }

  def row(rowNum: Int): Seq[CellState] = {
    for (c <- 0 to boardSizeMinusOne)
      yield cells(c)(rowNum)
  }

  def column(colNum: Int): Seq[CellState] = {
    for (r <- 0 to boardSizeMinusOne)
      yield cells(colNum)(r)
  }

  def diagonalOne: Seq[CellState] = {
    for (c <- 0 to boardSizeMinusOne)
      yield cells(c)(c)
  }

  def diagonalTwo: Seq[CellState] = {
    for (c <- 0 to boardSizeMinusOne)
      yield cells(c)(boardSizeMinusOne - c)
  }
  
  def rows: Seq[Seq[CellState]] = {
    for (r <- 0 to boardSizeMinusOne)
      yield row(r)
  }

  def columns: Seq[Seq[CellState]] = {
    for (c <- 0 to boardSizeMinusOne)
      yield column(c)
  }
  
  def allLines: Seq[Seq[CellState]] = {
    rows ++ columns :+ diagonalOne :+ diagonalTwo
  }

  def winner: CellState = {

    def winnerForLine(line: Seq[CellState]): Option[CellState] = {
      def cellOccupiedBy(player: CellState)(boardCell: CellState) = player == boardCell

      if (boardSize == line.count(cellOccupiedBy(X))) return Some(X)
      if (boardSize == line.count(cellOccupiedBy(O))) return Some(O)
      return None
    }
    
    for(line <- allLines) {
      val winnerOrNot = winnerForLine(line)
      if (winnerOrNot != None) return winnerOrNot.get
    }
    
    return Clear
  }

  def turnsPlayed: Int = {
    val playedCells = for (c <- 0 to boardSizeMinusOne) yield for (r <- 0 to boardSizeMinusOne)
      yield if (cells(c)(r) == Clear) None else Some(1)

    return playedCells.flatten.flatten.size
  }

  def gameOver: Boolean = (turnsPlayed == (boardSize * boardSize) || winner != Clear)

  def occupiedSquares(icon: CellState): Seq[(Int, Int)] = {
    val playedCells = for (c <- 0 to boardSizeMinusOne) yield for (r <- 0 to boardSizeMinusOne)
      yield if (cells(c)(r) == icon) Some(c, r) else None

    return playedCells.flatten.flatten
  }

  def emptySquares = occupiedSquares(Clear)

  def nextPlayer: CellState = {
    val xPlays = occupiedSquares(X).size
    val oPlays = occupiedSquares(O).size
    if (xPlays > oPlays) O else X
  }

}

object Board {

  def apply(): Board = {
    new Board(Array(
      Array(Clear, Clear, Clear),
      Array(Clear, Clear, Clear),
      Array(Clear, Clear, Clear)))
  }
  
//  def apply(row1: Array[CellState], row2: Array[CellState], row3: Array[CellState]): Board = {
//    new Board(Array(row1, row2, row3))
//  }
  
  def apply(row1: (CellState, CellState, CellState), row2: (CellState, CellState, CellState), row3: (CellState, CellState, CellState)): Board = {
    new Board(Array(
        Array(row1._1, row2._1, row3._1), 
        Array(row1._2, row2._2, row3._2), 
        Array(row1._3, row2._3, row3._3)))
  }
  
  def apply(cells: Array[Array[CellState]]): Board = {
    new Board(cells)
  }
  
  

  val corners = Seq(
    (0, 0),
    (0, 2),
    (2, 0),
    (2, 2))

}
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

  def col(colNum: Int): Seq[CellState] = {
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
  
  def winner: CellState = {

    def winnerForLine(line: Seq[CellState]): Option[CellState] = {
      def cellOccupiedBy(player: CellState)(boardCell: CellState) = player == boardCell
      
      if (boardSize == line.count(cellOccupiedBy(X))) return Some(X)
      if (boardSize == line.count(cellOccupiedBy(O))) return Some(O)
      return None
    }

    for (r <- 0 to boardSizeMinusOne) {
      val winnerOrNot = winnerForLine(row(r))
      if (winnerOrNot != None) return winnerOrNot.get
    }

    for (c <- 0 to boardSizeMinusOne) {
      val winnerOrNot = winnerForLine(col(c))
      if (winnerOrNot != None) return winnerOrNot.get
    }

    val winnerOrNot1 = winnerForLine(diagonalOne)
    if (winnerOrNot1 != None) return winnerOrNot1.get
        
    val winnerOrNot2 = winnerForLine(diagonalTwo)
    if (winnerOrNot2 != None) return winnerOrNot2.get

    return Clear
  }
  
  def turnsPlayed: Int = {
    val playedCells = for (c <- 0 to boardSizeMinusOne) yield
      for(r <- 0 to boardSizeMinusOne)
        yield if(cells(c)(r) == Clear) None else Some(1)
        
    return playedCells.flatten.flatten.size
  }
  
  def gameOver: Boolean = (turnsPlayed == (boardSize * boardSize) || winner != Clear)
  
  def occupiedSquares(icon: CellState): Seq[(Int, Int)] = {
    val playedCells = for (c <- 0 to boardSizeMinusOne) yield
      for(r <- 0 to boardSizeMinusOne)
        yield if(cells(c)(r) == icon) Some(c, r) else None 
        
    return playedCells.flatten.flatten
  }
  
  def emptySquares = occupiedSquares(Clear)
  
  def nextPlayer: CellState = {
    val xPlays = occupiedSquares(X).size
    val oPlays = occupiedSquares(O).size
    if(xPlays > oPlays) O else X
  }

}

object Board {

  def apply(): Board = {
    new Board(Array(
      Array(Clear, Clear, Clear),
      Array(Clear, Clear, Clear),
      Array(Clear, Clear, Clear)))
  }
  
  val corners = Seq(
    (0, 0),
    (0, 2),
    (2, 0),
    (2, 2))
  
}
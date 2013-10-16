package controllers.support

import ticTacToe.CellState._

object BoardState {
  
  def get(states: String): Seq[CellState] = {
    
    def stringToStates(states: String): Seq[CellState] = {
      states.toCharArray().toList.map(char => {
        char match {
          case 'A' => Clear
          case 'X' => X
          case 'O' => O
        }
      })
    }
    
    stringToStates(states);
  }

}
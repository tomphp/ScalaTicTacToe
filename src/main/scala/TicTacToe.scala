import scala.io

object TicTacToe {
  val lines = List(
    List(1, 2, 3),
    List(4, 5, 6),
    List(7, 8, 9),

    List(1, 4, 7),
    List(2, 5, 8),
    List(3, 6, 9),

    List(1, 5, 9),
    List(3, 5, 7)
  )

  def play {
    def loop(board: List[Int], player: Int): Int = {
      def draw() = {
        for (row <- 0 to 2) {
          for (col <- 0 to 2) {
            val cell = row * 3 + col

            print(" " + (board(cell) match {
              case 1 => "X"
              case 2 => "O"
              case _ => cell + 1
            }) + " ")
          }
          println
        }
      }

      def isFree(pos: Int): Boolean = {
        if (pos < 1 || pos > 9) false
        else board(pos - 1) == 0
      }

      def otherPlayer() = {
        if (player == 1) 2 else 1
      }

      def addPlay(pos: Int): (List[Int], Int) = {
        if (isFree(pos)) (board.updated(pos - 1, player), otherPlayer())
        else {
          println("Invalid move, try again")
          (board, player)
        }
      }

      def winner(): Int = {
        def loop(lines: List[List[Int]]): Int = {
          if (lines.isEmpty) 0
          else {
            val linePlayers = lines.head.map(c => board(c -1)).distinct

            if (linePlayers.size == 1 && linePlayers.head != 0) linePlayers.head
            else loop(lines.tail)
          }
        }

        loop(lines)
      }

      def gameOver(): Boolean = {
        winner() > 0 || !board.distinct.contains(0)
      }

      draw()

      if (gameOver()) {
        winner()
      }
      else {
        println("Player " + player + ", choose your next move")

        val move = readInt
        val (newBoard, newPlayer) = addPlay(move)

        loop(newBoard, newPlayer)
      }
    }

    val board = List(0, 0, 0, 0, 0, 0, 0, 0, 0)
    
    println(loop(board, 1) match {
      case 1 => "Player 1 won!"
      case 2 => "Player 2 won!"
      case _ => "Draw"
    })
  }
}

import scala.annotation.tailrec
import scala.io.StdIn.readInt

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
    @tailrec
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

      def isFree(pos: Int) = if (pos < 1 || pos > 9) false else board(pos - 1) == 0

      def otherPlayer = if (player == 1) 2 else 1

      def addPlay(pos: Int) = 
        if (isFree(pos)) Some((board.updated(pos - 1, player), otherPlayer))
        else None

      def lineWinner(line: List[Int]) = {
          val linePlayers = line.map(c => board(c - 1)).distinct

          if (linePlayers.size == 1) linePlayers.head
          else 0
      }

      def winner = lines.foldLeft(0)(
        (winner, line) =>
          if (winner != 0) winner
          else lineWinner(line)
      )

      def gameOver() = winner > 0 || !board.distinct.contains(0)

      draw()

      if (gameOver()) winner
      else {
        println("Player " + player + ", choose your next move")

        val move = readInt

        addPlay(move) match {
          case Some((newBoard, newPlayer)) => loop(newBoard, newPlayer)
          case None => {
            println("Invalid move, try again")
            loop(board, player)
          }
        }
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

package tictactoe

object Minimax {
  trait Tree[T, U] {
    def state: T
    def score: U
    def bestNext: Tree[T, U]
  }
  case class Node[T, U : Ordering](state: T, children: Seq[Tree[T, U]], player: Player) extends Tree[T, U] {
    lazy val score = player match {
      case Max => children.map(_.score).max
      case Min => children.map(_.score).min
    }

    lazy val bestNext = children.find(_.score == score).get
  }
  case class Leaf[T, U](state: T, score: U) extends Tree[T, U] {
    def bestNext = throw new NoSuchElementException
  }

  trait Player
  case object Max extends Player
  case object Min extends Player

  def minimax[T, U](state: T, grow: T => Seq[T], isEndState: T => Boolean, evaluate: T => U)(implicit ord: Ordering[U]) = {
    def iter(seed: T, currentPlayer: Player, nextPlayer: Player): Tree[T, U] =
      if (isEndState(seed)) Leaf(seed, evaluate(seed))
      else Node(seed, grow(seed) map (iter(_, nextPlayer, currentPlayer)), currentPlayer)

    iter(state, Max, Min)
  }
}

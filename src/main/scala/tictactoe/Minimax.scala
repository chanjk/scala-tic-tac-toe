package tictactoe

object Minimax {
  trait Tree[T, U] {
    def state: T
    def score: U
    def depthFactor: Int
    def winChance: (U, Int)
    def bestNext: Tree[T, U]
    def search(state: T): Option[Tree[T, U]]
  }
  case class Node[T, U : Ordering](state: T, children: Seq[Tree[T, U]], player: Player) extends Tree[T, U] {
    def search(state: T) = if (this.state == state) Some(this) else children.find(_.state == state)

    lazy val (score, depthFactor) = player match {
      case Max => children.map(x => (x.score, x.depthFactor)).max
      case Min => children.map(x => (x.score, x.depthFactor)).min
    }

    lazy val winChance = children.map(_.winChance).reduce[(U, Int)] {
      case ((s1, n1), (s2, n2)) => if (s1 == s2) (s1, n1 + n2) else player match {
        case Max => if (implicitly[Ordering[U]].lt(s1, s2)) (s1, n1) else (s2, n2)
        case Min => if (implicitly[Ordering[U]].gt(s1, s2)) (s1, n1) else (s2, n2)
      }
    }

    lazy val bestNext = {
      val candidates = scala.util.Random.shuffle(children).filter(x => (x.score, x.depthFactor) == (score, depthFactor))

      if (candidates.length == 1) candidates.head
      else player match {
        case Max => candidates.maxBy(_.winChance)
        case Min => candidates.minBy(_.winChance)
      }
    }
  }
  case class Leaf[T, U](state: T, score: U, depthFactor: Int, winChance: (U, Int)) extends Tree[T, U] {
    def search(state: T) = if (this.state == state) Some(this) else None
    def bestNext = throw new NoSuchElementException
  }

  trait Player
  case object Max extends Player
  case object Min extends Player

  def minimax[T, U](state: T, grow: T => Seq[T], isEndState: T => Boolean, evaluate: T => U)(implicit ord: Ordering[U]) = {
    def iter(state: T, currentPlayer: Player, nextPlayer: Player, depth: Int): Tree[T, U] =
      if (isEndState(state)) {
        val score = evaluate(state)

        currentPlayer match {
          case Max => Leaf(state, score, depth, (score, -1))
          case Min => Leaf(state, score, -depth, (score, 1))
        }
      }
      else Node(state, grow(state) map (iter(_, nextPlayer, currentPlayer, depth + 1)), currentPlayer)

    iter(state, Max, Min, 0)
  }
}

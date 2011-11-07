package euler.travis

import euler.Solution

import scalaz._
import Scalaz._

object Solutions extends euler.Solutions {
  val solutions = this.specify(
    Solution(1) {
      (0 until 1000).filter(x => x % 3 == 0 || x % 5 == 0).sum
    },

    Solution(2) {
      fibonacci[Int].filter(_ % 2 == 0).takeWhile(_ < 4000000).sum
    },

    Solution(3) {
      factors(600851475143L).head
    },

    Solution(4) {
      val q = new scala.collection.mutable.PriorityQueue[Int]
      for { i <- (100 until 1000); j <- (100 until i) } q += i * j
      while (!isPalindrome(q.head, 10)) q.dequeue
      q.head
    },

    Solution(4, "slow") {
      (for { i <- (100 until 1000); j <- (100 until i) } yield i * j)
        .toList.sortWith(_ > _).dropWhile(x => !isPalindrome(x, 10)).head
    },

    Solution(5) {
      (2 to 20).map(factorCounts(_)).foldLeft(Map.empty[Int, Int]) {
        (m, c) => (m /: c) {
          case (n, (k, v)) => n.updated(k, math.max(n.getOrElse(k, 0), v))
        }
      }.foldLeft(1) { case (n, (k, v)) => n * power(k, v) }
    },

    Solution(6) {
      power((1 to 100).sum, 2) - (1 to 100).map(power(_, 2)).sum
    },

    Solution(7) {
      primes[Int].drop(10000).head
    },

    Solution(7, "slow") {
      primesSlow[Int].drop(10000).head
    },

    Solution(8) {
      this.data(8).sliding(5).map(_.map(_.asDigit).product).max
    },

    Solution(9) {
      val (a, b, c) = (
        for { i <- 1 to 333; j <- i to 499 } yield (i, j, 1000 - i - j)
      ).filter {
        case (i, j, k) => i * i + j * j == k * k
      }.head
      a * b * c
    },

    Solution(10) {
      primes[BigInt].takeWhile(_ < 2000000).sum
    },

    Solution(10, "slow") {
      primesUpTo(2000000).view.map(BigInt(_)).sum
    },

    Solution(11) {
      val table = Function.uncurried(
        this.data(11).split("""\n""").map(_.split(" ").map(_.toInt).toSeq)
      ).tupled

      def candidates(x: Int, y: Int) = {
        val b = Seq.fill(4)(x, y)
        Seq(
          b.zipWithIndex.map { case ((i, j), k) => (i + k, j) },
          b.zipWithIndex.map { case ((i, j), k) => (i, j + k) },
          b.zipWithIndex.map { case ((i, j), k) => (i + k, j + k) },
          b.zipWithIndex.map { case ((i, j), k) => (i + k, j - k) }
        ).filter(_.all { case (i, j) => i > 0 && i < 20 && j > 0 && j < 20 })
      }

      (for {
        x <- (0 until 20)
        y <- (0 until 20)
        c <- candidates(x, y).map(_.map(table(_)).product)
      } yield c).max
    }
  )
}


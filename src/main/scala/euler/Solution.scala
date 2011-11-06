package euler

class Solution(val n: Int, v: => BigInt) extends scala.testing.Benchmark {
  lazy val value: BigInt = v

  // This is a possibly unnecessary precaution to keep the runtime from
  // compiling away the evaluation during benchmarking.
  private var current: Option[BigInt] = None

  def run: Unit = this.current = this.current match {
    case None => Some(this.value)
    case _    => Some(v)
  }

  override def toString = "Problem %4d           ".format(n)
}

class NamedSolution(n: Int, val name: String, v: => BigInt) extends Solution(n, v) {
  override def toString = "Problem %4d (%8s)".format(n, name)  
}

object Solution {
  def apply(n: Int)(v: => BigInt) = new Solution(n, v)
  def apply(n: Int, name: String)(v: => BigInt) = new NamedSolution(n, name, v)
}

trait Solutions {
  def solutions: Map[Int, Seq[Solution]]

  def specify(ss: Solution*) = (Map.empty[Int, Seq[Solution]] /: ss.map(s => s.n -> s)) {
    case (m, (i, s)) => m.updated(i, m.getOrElse(i, Seq.empty[Solution]) :+ s)
  }

  def data(n: Int): Option[String] =
    Option(this.getClass.getResource("/euler/data/%03d.txt".format(n))).map(
      scala.io.Source.fromURL(_).mkString
    )

  def benchmark(solutions: Seq[Solution]) { this.benchmark(solutions, 5) }
  def benchmark(solutions: Seq[Solution], i: Int) {
    solutions.foreach { solution =>
      val times = solution.runBenchmark(i)
      println("%s:%14d in %5d ms (%s)".format(
        solution.toString,
        solution.value,
        times.sum / times.size,
        times.map("%5d".format(_)).mkString(",")
      ))
    }
  }

  def main(args: Array[String]) {
    args match {
      case Array(IntString(n)) => this.benchmark(this.solutions(n))
      case _ => this.benchmark(this.solutions.toSeq.sortBy(_._1).flatMap(_._2))
    }
  }
}


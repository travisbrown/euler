package object euler {
  object IntString {
    val IntPattern = """(\d+)""".r
    def unapply(s: String): Option[Int] = s match {
      case IntPattern(n) => Some(n.toInt)
      case _             => None
    }
  }
}


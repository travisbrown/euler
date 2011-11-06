package euler

import scala.math.Integral.Implicits._
import scala.collection.immutable.BitSet

import scalaz._
import Scalaz._

package object travis {

def naturals[A](implicit f: Integral[A]): Stream[A] = Stream.iterate(f.one)(_ + f.one)
def power[A: Integral](a: A, b: Int): A = Stream.iterate(a, b)(_ * a).last
def probablyPrimes(c: Int) = naturals[BigInt].filter(_.isProbablePrime(c))

def primes[A: Integral]: Stream[A] = {
  val f = implicitly[Integral[A]]
  def sieve(n: A): Stream[A] = {
    def composite = primes.takeWhile { p =>
      f.lteq(f.times(p, p), n)
    }.exists { i => f.rem(n, i) == 0 }

    if (composite) sieve(f.plus(n, f.fromInt(2)))
    else n #:: sieve(f.plus(n, f.fromInt(2)))
  }
  f.fromInt(2) #:: sieve(f.fromInt(3))
}


def primesUpTo(n: Int): Seq[Int] = {
  val h = n / 2 + n % 2 - 1
  var bs = collection.mutable.BitSet.empty ++ (1 to h)
  var ps = collection.mutable.Buffer(2)

  var i = bs.min
  while ((i + 1) * i * 4 < n) {
    bs -= i
    ps += i * 2 + 1
    var j = (i + 1) * (i * 2)
    while (j * 2 < n) {
      bs.remove(j)
      j += i * 2 + 1
    }
    i = bs.min
  }
  ps ++ bs.map(_ * 2 + 1)
}

def fibonacci[A](implicit f: Integral[A]): Stream[A] =
  Stream.iterate((f.fromInt(1), f.fromInt(2))) {
    case (a, b) => (b, f.plus(a, b))
  }.map(_._1)

def factors[A](n: A)(implicit f: Integral[A]): List[A] = {
  var c = n
  var fs = List.empty[A]
  primes[A].takeWhile { p =>
    while (f.equiv(f.rem(c, p), f.fromInt(0))) {
      c = f.quot(c, p)
      fs = p :: fs
    }
    !f.equiv(c, f.fromInt(1))
  }.force
  fs
}

def factorCounts[A](n: A)(implicit f: Integral[A]): Map[A, Int] = {
  factors(n)(f).foldLeft(Map.empty[A, Int].withDefaultValue(0)) {
    case (m, i) => m.updated(i, m(i) + 1) 
  }
}

def digits[A](n: A, base: Int)(implicit f: Integral[A]): IndexedSeq[Int] = {
  Stream.iterate(1)(_ * base).takeWhile {
    i => f.lteq(f.fromInt(i), n)
  }.toIndexedSeq.scanRight((f.zero, n)) {
    case (i, (_, r)) => (f.quot(r, f.fromInt(i)), f.rem(r, f.fromInt(i)))
  }.dropRight(1).map(i => f.toInt(i._1))
}

def isPalindrome[A](n: A, base: Int)(implicit f: Integral[A]): Boolean = {
  val ds = digits(n, base)(f)
  ds == ds.reverse
}

}


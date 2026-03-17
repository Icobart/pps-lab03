package it.unibo.pps.lab03

// Task 1 (Svolto da solo)
import u03.Optionals.Optional

enum Sequence[E]:
  case Cons(head: E, tail: Sequence[E])
  case Nil()

object Sequence:

  def sum(l: Sequence[Int]): Int = l match
    case Cons(h, t) => h + sum(t)
    case _ => 0

  def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
    case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
    case Nil() => Nil()

  def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
    case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
    case Cons(_, t) => filter(t)(pred)
    case Nil() => Nil()

  @annotation.tailrec
  def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
    case Cons(_, t) if n > 0 => skip(t)(n - 1)
    case _ => s

  def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] =
    @annotation.tailrec
    def _zip(s1: Sequence[A], s2: Sequence[B], acc: Sequence[(A, B)]): Sequence[(A, B)] = (s1, s2) match
      case (Cons(h1, t1), Cons(h2, t2)) => _zip(t1, t2, Cons((h1, h2), acc))
      case _ => acc
    reverse(_zip(first, second, Nil()))

  def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] =
    @annotation.tailrec
    def _concat(con: Sequence[A], acc: Sequence[A]): Sequence[A] = con match
      case Cons(h, t) => _concat(t, Cons(h, acc))
      case _ => acc
    _concat(reverse(s1), s2)

  def reverse[A](s: Sequence[A]): Sequence[A] =
    @annotation.tailrec
    def _reverse(rev: Sequence[A], acc: Sequence[A]): Sequence[A] = rev match
      case Cons(h, t) => _reverse(t, Cons(h, acc))
      case _ => acc
    _reverse(s, Nil())

  def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
    @annotation.tailrec
    def _flatMap(flat: Sequence[A], acc: Sequence[B]): Sequence[B] = flat match
      case Cons(h, t) => _flatMap(t, concat(acc, mapper(h)))
      case _ => acc
    _flatMap(s, Nil())

  def min(s: Sequence[Int]): Optional[Int] =
    @annotation.tailrec
    def _min(seq: Sequence[Int], currentMin: Int): Optional[Int] = seq match
      case Cons(h, t) if h < currentMin => _min(t, h)
      case Cons(_, t) => _min(t, currentMin)
      case _ => Optional.Just(currentMin)
    s match
      case Cons(h, t) => _min(t, h)
      case _ => Optional.Empty()

  def evenIndices[A](s: Sequence[A]): Sequence[A] =
    @annotation.tailrec
    def _even(seq: Sequence[A], acc: Sequence[A]): Sequence[A] = seq match
      case Cons(h, Cons(_, t)) => _even(t, Cons(h, acc))
      case Cons(h, Nil()) => Cons(h, acc)
      case _ => acc
    reverse(_even(s, Nil()))

  def contains[A](s: Sequence[A])(elem: A): Boolean = s match
    case Cons(h, t) if h.equals(elem) => true
    case Cons(_, t) => contains(t)(elem)
    case _ => false

  def distinct[A](s: Sequence[A]): Sequence[A] = s match
    case Cons(h, t) => Cons(h, distinct(filter(t)(_ != h)))
    case _ => Nil()

  def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
    @annotation.tailrec
    def _group(seq: Sequence[A], currentGroup: Sequence[A], acc: Sequence[Sequence[A]]): Sequence[Sequence[A]] = seq match
      case Cons(h, t) => currentGroup match
        case Nil() => _group(t, Cons(h, Nil()), acc)
        case Cons(hc, _) if hc == h => _group(t, Cons(h, currentGroup), acc)
        case _ => _group(t, Cons(h, Nil()), Cons(currentGroup, acc))
      case Nil() => currentGroup match
        case Nil() => reverse(acc)
        case _ => reverse(Cons(currentGroup, acc))
    _group(s, Nil(), Nil())

  def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
    @annotation.tailrec
    def _partition(seq: Sequence[A], parYes: Sequence[A], parNo: Sequence[A]): (Sequence[A], Sequence[A]) = seq match
      case Cons(h, t) if pred(h) => _partition(t, Cons(h, parYes), parNo)
      case Cons(h, t) => _partition(t, parYes, Cons(h, parNo))
      case _ => (reverse(parYes), reverse(parNo))
    _partition(s, Nil(), Nil())

  // Task 2.2 (Svolto da solo)
  def foldLeft[A, B](s: Sequence[A])(defVal: B)(op: (B, A) => B): B = s match
    case Cons(h, t) => foldLeft(t)(op(defVal, h))(op)
    case _ => defVal

end Sequence

// Task 2 (Svolto da solo)
enum Person:
  case Student(name: String, year: Int)
  case Teacher(name: String, course: String)

object Person:
  import Sequence.*

  // Task 2.1
  def courses(ps: Sequence[Person]): Sequence[String] =
    flatMap(ps)(_ match
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()
    )

  // Task 2.3
  def countCourses(ps: Sequence[Person]): Int =
    val teachers = filter(ps)(_ match
      case Teacher(_, _) => true
      case _ => false
    )
    val extractedCourses = map(teachers)(_ match
      case Teacher(_, c) => c
      case _ => ""
    )
    foldLeft(distinct(extractedCourses))(0)((acc, _) => acc + 1)

end Person

// Task 3 (Svolto da solo)
enum Stream[A]:
  private case Empty()
  private case Cons(head: () => A, tail: () => Stream[A])

object Stream:
  import Sequence.*

  def empty[A](): Stream[A] = Empty()

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def toList[A](stream: Stream[A]): Sequence[A] = stream match
    case Cons(h, t) => Sequence.Cons(h(), toList(t()))
    case _ => Sequence.Nil()

  def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
    case Cons(head, tail) => cons(f(head()), map(tail())(f))
    case _ => Empty()

  def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
    case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
    case Cons(head, tail) => filter(tail())(pred)
    case _ => Empty()

  def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
    case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
    case _ => Empty()

  def iterate[A](init: => A)(next: A => A): Stream[A] =
    cons(init, iterate(next(init))(next))

  // Task 3.6
  def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
    case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
    case _ => Empty()

  // Task 3.7
  def fill[A](n: Int)(k: A): Stream[A] = n match
    case n if n <= 0 => Empty()
    case _ => cons(k, fill(n - 1)(k))

  // Task 3.8
  val fibonacci: Stream[Int] =
    def _fib(a: Int, b: Int): Stream[Int] = cons(a, _fib(b, a + b))
    _fib(0, 1)

  // Task 3.9
  def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = s1 match
    case Cons(head, tail) => cons(head(), interleave(s2, tail()))
    case _ => s2

  // Task 3.10
  def cycle[A](list: Sequence[A]): Stream[A] =
    def _cycle(curr: Sequence[A]): Stream[A] = curr match
      case Sequence.Cons(h, t) => Stream.cons(h, _cycle(t))
      case _ => _cycle(list)
    list match
      case Nil() => Empty()
      case _ => _cycle(list)

end Stream
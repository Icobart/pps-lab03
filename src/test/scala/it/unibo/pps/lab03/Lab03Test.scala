package it.unibo.pps.lab03

import org.junit.*
import org.junit.Assert.*
import u03.Optionals.Optional.{Empty, Just}
import Sequence.*
import Person.*
import Stream.*

// Test Task 1 (e Task 2.2)
class SequenceTest:

  val sequence: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(sequence))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), Sequence.map(sequence)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), Sequence.map(sequence)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), Sequence.filter(sequence)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), Sequence.filter(sequence)(_ != 20))

  @Test def testSkip() =
    assertEquals(Cons(30, Nil()), skip(sequence)(2))
    assertEquals(Nil(), skip(sequence)(3))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), skip(sequence)(0))
    assertEquals(Nil(), skip(Nil())(2))

  @Test def testZip() =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(sequence, l2))
    assertEquals(Nil(), zip(sequence, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(sequence, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

  @Test def testReverse() =
    assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverse(sequence))
    assertEquals(Nil(), reverse(Nil()))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(sequence)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin() =
    assertEquals(Just(10), min(sequence))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))

  @Test def testEvenIndices() =
    assertEquals(Cons(10, Cons(30, Nil())), evenIndices(sequence))
    assertEquals(Nil(), evenIndices(Nil()))

  @Test def testContains() =
    assertEquals(true, contains(sequence)(10))
    assertEquals(false, contains(sequence)(15))
    assertEquals(false, contains(Nil())(10))

  @Test def testDistinct() =
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), distinct(sequence))
    assertEquals(Cons(10, Cons(20, Nil())), distinct(Cons(10, Cons(20, Cons(10, Nil())))))
    assertEquals(Nil(), distinct(Nil()))

  @Test def testGroup() =
    val sequence = Cons(10, Cons(10, Cons(20, Cons(30, Cons(20, Nil())))))
    val grouped =
      Cons(Cons(10, Cons(10, Nil())), Cons(Cons(20, Nil()), Cons(Cons(30, Nil()), Cons(Cons(20, Nil()), Nil()))))
    assertEquals(group(sequence), grouped)
    assertEquals(Nil(), group(Nil()))

  @Test def testPartition() =
    val sequence = Cons(11, Cons(20, Cons(31, Nil())))
    val (even, odd) = partition(sequence)(x => x % 2 == 0)
    assertEquals(Cons(20, Nil()), even)
    assertEquals(Cons(11, Cons(31, Nil())), odd)

    val emptySequence = Nil()
    val (evenEmpty, oddEmpty) = partition(emptySequence)(x => true)
    assertEquals(Nil(), evenEmpty)
    assertEquals(Nil(), oddEmpty)

  // Task 2.2
  @Test def testFoldLeft() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    val expectedSub = -16
    val defaultValue = 0
    assertEquals(expectedSub, foldLeft(lst)(defaultValue)(_ - _))
    val expectedAdd = 16
    assertEquals(expectedAdd, foldLeft(lst)(defaultValue)(_ + _))


// Test Task 2
class Task2Test:

  @Test def testCoursesAndCount() =
    val persons: Sequence[Person] = Cons(
      Teacher("Viroli", "PPS"),
      Cons(
        Teacher("Aguzzi", "PPS"),
        Cons(
          Teacher("Ricci", "PCD"),
          Cons(
            Student("Mario", 2015),
            Nil()
          )
        )
      )
    )

    val expectedCourses = Cons("PPS", Cons("PPS", Cons("PCD", Nil())))
    assertEquals(expectedCourses, courses(persons))

    val expectedCount = 2
    assertEquals(expectedCount, countCourses(persons))


// Test Task 3
class StreamTest:

  @Test def testTakeWhile() =
    val stream = Stream.iterate(0)(_ + 1)
    val result = Stream.toList(Stream.takeWhile(stream)(_ < 5))
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), result)

  @Test def testFill() =
    val result = Stream.toList(Stream.fill(3)("a"))
    assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), result)
    assertEquals(Nil(), Stream.toList(Stream.fill(0)("a")))

  @Test def testFibonacci() =
    val result = Stream.toList(Stream.take(Stream.fibonacci)(5))
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil()))))), result)

  @Test def testInterleave() =
    val s1 = Stream.take(Stream.iterate(1)(_ + 2))(3)
    val s2 = Stream.take(Stream.iterate(2)(_ + 2))(5)
    val result = Stream.toList(Stream.interleave(s1, s2))
    val expected = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(8, Cons(10, Nil()))))))))
    assertEquals(expected, result)

  @Test def testCycle() =
    val seq = Cons('a', Cons('b', Cons('c', Nil())))
    val repeatStream = Stream.cycle(seq)
    val result = Stream.toList(Stream.take(repeatStream)(5))
    val expected = Cons('a', Cons('b', Cons('c', Cons('a', Cons('b', Nil())))))
    assertEquals(expected, result)

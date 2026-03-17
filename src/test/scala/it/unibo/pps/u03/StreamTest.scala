package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*
import u03.Streams.*
import u03.Sequences.*
import Sequence.*

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

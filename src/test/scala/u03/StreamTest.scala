package u03

import org.junit.*
import org.junit.Assert.*
import u03.Streams.*
import u03.Streams.Stream.*
import u03.Lists.List.*

class StreamTest:

  val s = Stream.take(Stream.iterate(0)(_ + 1))(10)


  @Test def testDrop() =
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), toList( Stream.drop(s)(6)))

  @Test def testConstant() =
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), toList(Stream.take(constant("x"))(5)))

  @Test def testFibs() =
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))) , Stream.toList(Stream.take(fibs)(8)))







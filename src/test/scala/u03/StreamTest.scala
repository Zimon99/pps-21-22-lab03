package u03

import org.junit.*
import org.junit.Assert.*
import u03.Streams.Stream
import u03.Streams.Stream.*

class StreamTest:

  var str = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  var str1 = Stream.take(str)(10) // {0,1,2,3, .. ,9}

  @Test def testDrop() =
    assertEquals(toList(cons(9, empty())), toList( drop(str1)(9)))

  



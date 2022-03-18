package u03

import org.junit.*
import org.junit.Assert.*
import u02.Optionals.*
import u02.Modules.Person
import u03.Lists.List

class ListTest:
  import List.*


  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val l1: List[Int] = Cons(50, Nil())
  val l2: List[Person] = Cons(Person.Teacher("P1", "C1"), Nil())
  val l3: List[Person] = Cons(Person.Teacher("P1", "C1"), Cons(Person.Teacher("P2", "C2"), Cons(Person.Teacher("P3", "C3"),Nil())))
  val l4: List[Person] = Cons(Person.Student("P1", 23), Cons(Person.Teacher("P2", "C2"), Cons(Person.Teacher("P3", "C3"),Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 3))
    assertEquals(Nil(), drop(l, 5))

  @Test def testAppend() =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(50, Nil())))), append(l, l1))

  @Test def testFlatMap() =
    assertEquals(Cons (11 , Cons(21 , Cons(31 , Nil ()))), flatMap(l)(v => Cons(v + 1, Nil())) )
    assertEquals( Cons (11 , Cons(12 , Cons(21 , Cons(22 , Cons(31 , Cons (32 , Nil())))))) ,flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil() ))))

  @Test def testMapWithFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapWithFlatMap(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapWithFlatMap(l)(_ + ""))

  @Test def testFilterWithFlatMap() =
    assertEquals(Cons(20, Cons(30, Nil())), filterWithFlatMap(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterWithFlatMap(l)(_ != 20))

  @Test def testMax() =
    import u02.Optionals.Option.*
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(max(Nil()), Option.None())

  def testForExtractCourses(f: List[Person] => List[String]) =
    assertEquals(Cons("C1", Nil()), f(l2))
    assertEquals(Cons("C1", Cons("C2", Cons("C3", Nil()))), f(l3))
    assertEquals(Cons("C2", Cons("C3", Nil())), f(l4))

  @Test def testExtractCoursesFromPersonsWithoutFlatMap() =
    testForExtractCourses(extractCoursesFromPersonsWithoutFlatMap)

  @Test def testExtractCoursesFromPersonsWithFlatMap1() =
    testForExtractCourses(extractCoursesFromPersonsWithFlatMap1)

  @Test def testExtractCoursesFromPersonsWithFlatMap2() =
    testForExtractCourses(extractCoursesFromPersonsWithFlatMap2)

  @Test def testFoldLeft() =
    assertEquals(-16, foldLeft(Cons(3,Cons(7,Cons(1,Cons(5, Nil())))))(0)(_ - _))

  @Test def testReverseList() =
    assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverseList(l))

  @Test def testFoldRight() =
    assertEquals(-8, foldRight(reverseList(Cons(3,Cons(7,Cons(1,Cons(5, Nil()))))))(0)(_ - _))
package u03

import u02.Modules.Person.Teacher

object Lists extends App:

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    // Es 1a
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h, t) if n > 0 => drop(t, n-1)
      case Cons(h, t) => Cons(h, t)
      case Nil() => Nil()

    // Es 1b
    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case _ => right

    // Es 1c
    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()

    // Es 1d
    def mapWithFlatMap[A, B](l: List[A])(mapper: A => B): List[B] = flatMap(l)(v => Cons(mapper(v), Nil()))

    // Es 1d
    def filterWithFlatMap[A](l1: List[A])(pred: A => Boolean): List[A] =
      //flatMap(l1)(v => if pred(v) then Cons(v, Nil()) else Nil())
      val caster = (v: A) => pred(v) match
        case true => Cons(v, Nil())
        case false => Nil()
      flatMap(l1)(caster)

    // Es 2
    import u02.Optionals.*
    import u02.Optionals.Option.*
    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) if h >= orElse(max(t), h) => Option.Some(h)
      case Cons(h, t) => max(t)
      case Nil() => Option.None()

    // Es3
    import u02.Modules.Person
    def extractCoursesFromPersons[A](l: List[Person]): List[String] =
      // Soluzione senza flatMap (con match sulla l)
      //case Cons(Person.Teacher(n, c), t) => Cons(c, extractCoursesFromPersons(t))
      //case Cons(Person.Student(n, c), t) => extractCoursesFromPersons(t)
      //case Nil() => Nil()

      // Soluzione con flatmap e pred dichiarata prima.
      val pred = (x: Person) => x match
        case Person.Teacher(n, c) => Cons(c, Nil())
        case Person.Student(n, c) => Nil()
      flatMap(l)(pred)

      // Soluzione con flatmap e pred integrata come body.
      //flatMap(l)({ case Person.Teacher(n, c) => Cons(c, Nil()) ; case Person.Student(n, c) => Nil() })

      // Es4











  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52



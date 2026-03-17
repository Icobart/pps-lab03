package it.unibo.pps.u02

object Modules extends App :

  // An ADT: type + module
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

  println(Person.name(Person.Student("mario", 2015)))

  import Person.*
  import u03.Sequences.*
  import Sequence.*

  println(name(Student("mario", 2015)))

  // a method outside the Person module
  def isStudent(p: Person): Boolean = p match
    case Student(_, _) => true
    case _ => false

  println(isStudent(Student("mario", 2015)))

  // Task 2.1
  def courses(ps: Sequence[Person]): Sequence[String] =
    flatMap(ps)(_ match
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()
    )


  val testPersons = Cons(Teacher("Viroli", "PPS"), Cons(Teacher("Aguzzi", "PPS"),
                    Cons(Teacher("Ricci", "PCD"), Cons(Student("Mario", 2015), Nil()))))
  println("Courses: "+ courses(testPersons))
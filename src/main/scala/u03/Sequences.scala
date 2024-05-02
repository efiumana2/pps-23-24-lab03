package u03

//import u02.AnonymousFunctions.l
import u03.Optionals.*
import Optional.*
import u03.Persons.Person.Teacher
//import math.*

//import scala.collection.immutable.Stream.Empty

object Sequences: // Essentially, generic linkedlists

  import Persons.*

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    extension [A](l: Sequence[A])

      def map[B](mapper: A => B): Sequence[B] = l match
        case Cons(h, t) => Cons(mapper(h), t.map(mapper))
        case Nil() => Nil()

      def filter(pred: A => Boolean): Sequence[A] = l match
        case Cons(h, t) if pred(h) => Cons(h, t.filter(pred))
        case Cons(_, t) => t.filter(pred)
        case Nil() => Nil()

//Task 2
      def foldLeft(dflt: A,mapper: (A,A) => A):A = l match
        case Cons(h,t) => t.foldLeft(mapper(dflt,h),mapper)
        case Nil() => dflt

    extension (l: Sequence[Person])
      def GetCourses: Sequence[String] =
        filter(l)(v => v match
          case Teacher(_,c)=>true
          case (_) => false).map(v => v match
          case Teacher(_,c) => c)

/*    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()*/

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0


    // Lab 03 - Task 1
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = first match
      case Cons(h, t) => second match
        case Cons(h2, t2) => Cons((h,h2),zip(t,t2))
        case Nil()        => Nil()
      case Nil()      => Nil()
      
    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(h, t) if n > 0  => Cons(h,take(t)(n-1))
      case _                    => Nil()

    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case Cons(h, t)   => Cons(h,concat(t,l2))
      case Nil()        => l2 

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(h, t)   => concat(mapper(h), flatMap(t)(mapper))
      case Nil()        => Nil()

    def min(l: Sequence[Int]): Optional[Int] =  // Mifaccio una funzione interna che mi calcola il minimo e ha come parametro anche il miglior minimo trovato finora
      def minInt(c: Sequence[Int], TempMin: Int): Optional[Int] = c match
        case Cons(h, t) if h < TempMin              => minInt(t,h)
        case Cons(_, t)                             => minInt(t,TempMin)
        case Nil()      if TempMin == Int.MaxValue  => Optional.Empty()
        case _                                      => Just(TempMin)
      minInt(l,Int.MaxValue)



object Persons:
  import Sequences.*
  enum Person: // a sum type defined by enumerating various cases
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

@main def trySequences =
  import 
  Sequences.*
  import Persons.*

  val lp = Sequence.Cons(Person.Teacher("io","sysint"),
           Sequence.Cons(Person.Teacher("Mirko","pps"),
           Sequence.Cons(Person.Teacher("Alessandro","pcd"),
             Sequence.Cons(Person.Student("Enrico",2023),
               Sequence.Nil()))))
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(flatMap(l)(v => Cons (v + 1, Cons (v + 2, Nil ()))))
  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
  println(Int.MaxValue)
  println(GetCourses(lp))

package fp

import java.time.LocalTime

import scalaz._
import Scalaz._
import scala.{Ordering => SOrdering}
import scalaz.Alpha.F
import scalaz.concurrent.Task

object QuickSort {

  trait Order[A] { self =>
    def compare(a1: A, a2: A): Int

    def revert: Order[A] = new Order[A] {
      def compare(a1: A, a2: A) = - self.compare(a1, a2)
    }

  }


  def lessOrEqual[A](a: A, l: List[A])(implicit order: Order[A]): List[A] = {
    l.filter(r => order.compare(r, a) <= 0)
  }

  def greate[A](a: A, l: List[A])(implicit order: Order[A]): List[A] = {
    l.filter(r => order.compare(r, a) > 0)
  }

//  def sort[A](l: List[A])(implicit order: Order[A]): List[A] = l match {
//    case Nil => Nil
//    case r :: tail =>
//      sort(lessOrEqual(r, tail)) ++ List(r) ++ sort(greate(r, tail))
//  }

  def sort2[A : Order](l: List[A]): List[A] = l match {
    case Nil => Nil
    case r :: tail =>
      sort2(lessOrEqual(r, tail)) ++ List(r) ++ sort2(greate(r, tail))
  }
}

object Sample extends App {

  import Domain._

  val room = Room("1", 1, 1, true, 1, 1, List(Reservation(1, Period(LocalTime.now(), LocalTime.now()), Guest("", ""))))
  val rooms = (0 to 10).map(i => room.copy(rating = Math.random())).toList
  implicit def order: QuickSort.Order[Room] = new QuickSort.Order[Room] {
    override def compare(a1: Room, a2: Room): Int = if (a1.rating > a2.rating) 1 else if (a1.rating < a2.rating) -1 else 0
  }.revert
  QuickSort.sort2(rooms).foreach(println)
}

object Domain {
  type NoPpl = Int
  type ReservationId = Int
  type Price = Double

  case class Period(from: LocalTime, to: LocalTime)
  case class Guest(firstName: String, lastName: String)
  case class Reservation(id: ReservationId, period: Period, guest: Guest)

  case class Room(no: String, floor: Int, price: Double, view: Boolean, capacity: Int, rating: Double, booked: List[Reservation])
  case class Booking(rooms: List[Room] = Nil)
}

object SandBox {
  import Domain._
  import Functions._


  type Error[A] = String \/ A
  def parse(json: String): Error[Room] = ???
  val cpp: Error[Double] = costPerPerson2[Error].apply(parse(""))

  def getFromDB(id: String): Task[Room] = ???
  val cpp2: Task[Double] = costPerPerson2[Task].apply(getFromDB(""))
}


object Functions2 {
  import Domain._
  import Functions._

  val isAffordable: (Room, Price) => Boolean = (room: Room, price: Price) => room.price <= price

  def affordableFor[F[_] : Applicative](room: F[Room], price: Price): F[Boolean] = {
    val f: F[Price => Boolean] = room.map(isAffordable.curried)
    Applicative[F].ap(price.point[F])(f)
  }

  def affordableFor2[F[_] : Apply](room: F[Room], price: F[Price]): F[Boolean] = {
    val f: F[Price => Boolean] = room.map(isAffordable.curried)
    Apply[F].ap(price)(f)
  }

  def affordableForApplicativeScalaZ[F[_] : Applicative](room: F[Room], price: Price): F[Boolean] = {
    val f: F[Price => Boolean] = room.map(isAffordable.curried)
    Apply[F].ap(price.point[F])(f)
    price.point[F] <*> f
  }

  def affordableShort[F[_]: Apply](room: F[Room], price: F[Price]): F[Boolean] = {
    Apply[F].apply2(room, price)(isAffordable)
  }

  def affordableShort2[F[_]: Applicative](room: F[Room], price: F[Price]): F[Boolean] = {
    (room |@| price)(isAffordable)
  }

  def bestFor[F[_]: Applicative](booking: F[Booking], period: F[Period], noPlp: F[NoPpl]): F[Option[Room]] = {
    (booking |@| period |@| noPlp)(proposeBest)
  }

  def bestFor2[F[_]: Bind](booking: F[Booking], fetchPeriod: Booking => F[Period], fetchNoPlp: Booking => F[NoPpl]): F[Option[Room]] = {
    val period: F[Period] = Bind[F].bind(booking)(fetchPeriod)
    val noPlp: F[NoPpl] = Bind[F].bind(booking)(fetchNoPlp)
    (booking |@| period |@| noPlp)(proposeBest)
  }

  def bestFor2Short[F[_]: Bind](booking: F[Booking], fetchPeriod: Booking => F[Period], fetchNoPlp: Booking => F[NoPpl]): F[Option[Room]] = {
    booking >>= (b => fetchPeriod(b) >>= (p => fetchNoPlp(b).map(n => proposeBest(b, p, n))))
  }
  def bestFor2Short2[F[_]: Bind](booking: F[Booking], fetchPeriod: Booking => F[Period], fetchNoPlp: Booking => F[NoPpl]): F[Option[Room]] = {
    for {
      b <- booking
      p <- fetchPeriod(b)
      n <- fetchNoPlp(b)
    } yield proposeBest(b, p, n)
  }
 }

object Functions {

  import Domain._

  implicit def roomOrdering: SOrdering[Room] = new SOrdering[Room] {
    def compare(r1: Room, r2: Room): Int = {
      (r1.rating - r2.rating).toInt
    }
  }

  def filter(predicate: Room => Boolean): List[Room] => List[Room] = {
    case rooms => rooms.filter(predicate)
  }

  val costPerPerson: Option[Room] => Option[Double] = {
    case room => room.map(room => room.price / room.capacity)
  }
  def costPerPerson2[F[_]: Functor]: F[Room] => F[Double] = {
    case room => room.map(room => room.price / room.capacity)
  }

  val f1: (Int, String) => String = (i, s) => s"s=$s; i=$i"
  val f2: ((Int, String)) => String = f1.tupled
  val f3: (Int, String) => String = Function.untupled(f2)

  val pickAvaiable: (Period, List[Room]) => List[Room] = {
    case (period, rooms) => rooms.filter { room =>
      !room.booked.map(_.period).contains(period)
    }
  }


  val filterWithView: List[Room] => List[Room] = (rooms: List[Room]) => rooms.filter(_.view)
  val filterCanAccomodate: (NoPpl, List[Room]) => List[Room] = (noPpl, rooms) => rooms.filter(_.capacity >= noPpl)
  val sortByRating: List[Room] => List[Room] = (rooms: List[Room]) => rooms.sorted

  val   proposeBest: (Booking, Period, NoPpl) => Option[Room] = {
    case (Booking(rooms), period, noPlp) => {
      val pickForPeriod: List[Room] => List[Room] = pickAvaiable.curried(period)
      val filterForNoPpl: List[Room] => List[Room] = filterCanAccomodate.curried(noPlp)
      val propose: List[Room] => Option[Room] =
        pickForPeriod >>> filterWithView >>> filterForNoPpl >>> sortByRating >>> {
          case r::_ => Some(r)
          case _ => None
        }
      propose(rooms)
    }
  }

  val costPerPersonForBest2: (Booking, Period, NoPpl) => Option[Double] =
    Function.untupled(proposeBest.tupled andThen costPerPerson2)

//  val proposeBest2: Booking => Room =
//    ((b: Booking) => b.rooms) >>> pickAvaiable andThen filterWithView andThen sortByRating andThen (rooms => rooms.head)

}
package fp

import scalaz.concurrent.Task
import scalaz.{Free, ~>}
import org.slf4j.LoggerFactory

sealed trait Logging[A]

case class Info(line: String) extends Logging[Unit]
case class Warn(line: String) extends Logging[Unit]
case class Error(line: String) extends Logging[Unit]
case class Debug(line: String) extends Logging[Unit]

object Logging {
  def info(line: String): Free[Logging, Unit] = Free.liftF(Info(line))
  def warn(line: String): Free[Logging, Unit] = Free.liftF(Warn(line))
  def error(line: String): Free[Logging, Unit] = Free.liftF(Error(line))
  def debug(line: String): Free[Logging, Unit] = Free.liftF(Debug(line))
}

object LoggingInterpreter extends (Logging ~> Task) {
  override def apply[A](logging: Logging[A]): Task[A] = logging match {
    case Info(line) => Task.delay {
      LoggerFactory.getLogger(this.getClass).info(line)
    }
    case Warn(line) => Task.delay {
      LoggerFactory.getLogger(this.getClass).warn(line)
    }
    case Error(line) => Task.delay {
      LoggerFactory.getLogger(this.getClass).error(line)
    }
    case Debug(line) => Task.delay {
      LoggerFactory.getLogger(this.getClass).debug(line)
    }
  }
}

object FreeAppLoging extends App {
  import Logging._

  val programm: Free[Logging, Unit] = for {
    _ <- info("start")
    _ <- error("error !!!")
  } yield ()

  val task: Task[Unit] = programm.foldMap(LoggingInterpreter)
  task.unsafePerformSync

}

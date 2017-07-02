package fp

import scalaz.{Free, ~>}
import scalaz.concurrent.Task

sealed trait InOut[A]

case class PrintLine(line: String) extends InOut[Unit]

case object GetLine extends InOut[String]

object InOut {
  def printLine(line: String): Free[InOut, Unit] = Free.liftF(PrintLine(line))
  def getLine(): Free[InOut, String] = Free.liftF(GetLine)
}

object ConsoleInterpreter extends (InOut ~> Task) {
  override def apply[A](inOut: InOut[A]): Task[A] = inOut match {
    case PrintLine(line) => Task.delay {
      println(line)
    }
    case GetLine => Task.delay {
      scala.io.StdIn.readLine()
    }
  }
}

object FreeApp extends App {

  import InOut._

  val programm: Free[InOut, Unit] = for {
    _ <- printLine("What's your name?")
    name <- getLine()
    _ <- printLine(s"Nice to meet you $name")
  } yield ()

  val task: Task[Unit] = programm.foldMap(ConsoleInterpreter)
  task.unsafePerformSync
}

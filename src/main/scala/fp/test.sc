val f1: Int => String = (i: Int) => {
  println(s"f1 i = $i")
  "a " + i
}
val f2: String => String = (i: String) => {
  println(s"f2 i=$i")
  "b " + i
}

val f3 = f1 andThen f2

f3(10)

println("===================")
val f4 = (f2 compose f1)(100)





val f5: (String => String) = ((x: String) => x.toInt) andThen f1 andThen f2

f5("-9")

val f6: String => Array[String] = ((s: String) => s.split(' ')) compose f5


f6("123")
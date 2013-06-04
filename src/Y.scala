import scala.collection.mutable.ListBuffer

object Y extends App {

  val l = new ListBuffer[Int] ++ List(1,1,2,2,2,3)

  l remove 3
  println(l)

}

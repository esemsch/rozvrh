object Y extends App {

  val m = Map[String,Int]() + ("x" -> -10)

  if(m.get("x").exists(x => x<0)) println("Ahoj")

}

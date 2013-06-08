package x

object H extends App {

  def order = {

    val h = """Ch 9 (Lucka) ----> 261504
      |Čj 8/9 (Bohunka) ----> 33264.......
      |Inf 8 (Eva) ----> 630816...
      |Čj 6 (Hana) ----> 288336
      |Z 6/7 (Hana) ----> 38364
      |Př 8/9 (Lucka) ----> 19260........
      |Aj 3 (Martina) ----> 756960
      |Aj 6/7 (Tereza) ----> 7452......
      |Čj 9 (Bohunka) ----> 630816
      |Prv 3/Př 4 (Tereza) ----> 0.
      |Z 8 (Hana) ----> 106464.
      |Aj 8 (Tereza) ----> 69408....
      |Vl 4/5 (Tereza) ----> 378480...
      |Z 7 (Hana) ----> 288336
      |F/M 8/9 (Alena) ----> 10356.
      |F 8 (Alena) ----> 145344.
      |Z 9 (Hana) ----> 106464.....
      |Př 6 (Lucka) ----> 466464.
      |F 9 (Alena) ----> 145344
      |M/F 8/9 (Alena) ----> 10356...
      |Čj 7 (Hana) ----> 288336.
      |Aj 7 (Tereza) ----> 116784...
      |Ch 8 (Lucka) ----> 261504......
      |M 2/4 (Gita) ----> 571452......
      |Inf 9 (Eva) ----> 630816........
      |Čj 3 (Martina) ----> 756960....
      |Čj 3/5 (Martina) ----> 478698...
      |D 6 (Hana) ----> 288336......
      |Čj 2/4 (Gita) ----> 571452...
      |D 7 (Hana) ----> 288336.
      |D 9 (Hana) ----> 106464...........
      |Vl 5 (Tereza) ----> 1135440...
      |Př 9 (Lucka) ----> 261504.
      |D 8 (Hana) ----> 106464........
      |Čj 2 (Gita) ----> 756960......
      |Čj 8 (Bohunka) ----> 630816
      |D 6/7 (Hana) ----> 38364....
      |F 7 (Alena) ----> 374496......
      |Prv/Př  2/4 (Gita) ----> 571452....
      |F 6 (Alena) ----> 374496
      |Aj 8/9 (Tereza) ----> 3420..
      |Z 6 (Hana) ----> 288336.
      |Ch 8/9 (Lucka) ----> 19260.
      |Aj 6 (Tereza) ----> 116784..
      |Př 8 (Lucka) ----> 261504.
      |Př 6/7 (Lucka) ----> 38088.
      |M 8 (Alena) ----> 145344....
      |M 7 (Alena) ----> 374496
      |M 6/7 (Alena) ----> 37812.
      |Čj 6/7 (Hana) ----> 38364
      |M 8/9 (Alena) ----> 10356
      |M/F 6/7 (Alena) ----> 37812
      |Aj 4/5 (Martina) ----> 0.....
      |Př 7 (Lucka) ----> 466464.....
      |M 3/5 (Martina) ----> 478698
      |F/M 6/7 (Alena) ----> 37812.
      |Aj 9 (Tereza) ----> 69408
      |Z 8/9 (Hana) ----> 7380....
      |Vl 4 (Tereza) ----> 378480.
      |M 9 (Alena) ----> 145344....
      |M 6 (Alena) ----> 374496
    """.stripMargin

    val order: List[(String, Int)] = """(.*) ----> (\d+).*""".r.findAllIn(h).matchData.map(mch => {
      (mch.group(1), mch.group(2).toInt)
    }).toList.sortWith((f, s) => f._2 <= s._2)

//    println(order.mkString("\n"))

    order
  }
}

class HOrder(tiles:Seq[Tile],h:List[(String,Int)]) {

  val order = {
    val aux = new Array[Int](1000)
    (1 to 1000).foreach(i => aux(i-1) = 1000)
    h.foreach(pair => {
      val aux2 = tiles.filter(t => pair._1.contains(t.job.classHour.subject) && pair._1.contains(t.job.teacher.name))
      aux2.foreach(t => aux(t.id) = pair._2)
    })
    aux
  }

  def precedes(t1:Tile,t2:Tile) = {
    order(t1.id) > order(t2.id)
  }
}

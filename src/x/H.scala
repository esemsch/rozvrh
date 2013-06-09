package x

object H extends App {

  def parseOrder(h:String) = {"""(.*) ----> (\d+).*""".r.findAllIn(h).matchData.map(mch => {
      (mch.group(1), mch.group(2).toInt)
    }).toList.sortWith((f, s) => f._2 <= s._2)
  }

  def order = {

    val h = """Prv 3/Př 4 (Tereza) ----> 1174272.........................................
              |Prv 3 (Tereza) ----> 4063968...........
              |Aj 4/5 (Martina) ----> 1128288...................................................................
              |Prv 2 (Gita) ----> 6693120..
              |Vo 6/7 (Lucka) ----> 204498..
              |Rv 8/9 (Eva) ----> 170838................
              |Pč 3/5 (Eva) ----> 1586568.............................................
              |Spv 8 (Eva) ----> 4506048..
              |Pč 6/7 (Eva) ----> 212988..........................
              |Hv 2/4 (Gita) ----> 2655450.
              |Vv 6/7 (Tereza) ----> 29670
              |Hv 8/9 (Hana) ----> 66312..
              |Pč 8/9 (Eva) ----> 170838............................................
              |Vv 4 (Tereza) ----> 4413552........................................
              |Vv 5 (Martina) ----> 3949008..
              |Hv 6/7 (Hana) ----> 204498....................................................
              |SPV 9 (Bohunka) ----> 5185464....
              |Hv 3/5 (Tereza) ----> 482940....
              |Vv 2/4 (Tereza) ----> 377190............................................................................
              |Rj 7 (Iva) ----> 7618608..
              |Vo 8/9 (Eva) ----> 170838...........................
              |Pč 2/4 (Gita) ----> 2655450..............................
              |Rv 7 (Lucka) ----> 3033984..................................
              |Vv 3/5 (Martina) ----> 3432684.................
              |Inf 5/6 (Eva) ----> 1697808.
              |Vv 8/9 (Tereza) ----> 18930
              |Aj 8/9 (Tereza) ----> 18930
              |Z 8/9 (Hana) ----> 66312.
              |Aj 6/7 (Tereza) ----> 29670.
              |F/M 8/9 (Alena) ----> 91302.
              |M/F 8/9 (Alena) ----> 91302
              |M 8/9 (Alena) ----> 91302..
              |Př 8/9 (Lucka) ----> 114396.
              |Ch 8/9 (Lucka) ----> 114396..
              |Čj 8/9 (Bohunka) ----> 206904..
              |M 6/7 (Alena) ----> 203388..
              |F/M 6/7 (Alena) ----> 203388..
              |M/F 6/7 (Alena) ----> 203388..
              |Př 6/7 (Lucka) ----> 204498..
              |Z 6/7 (Hana) ----> 204498..
              |Čj 6/7 (Hana) ----> 204498..
              |D 6/7 (Hana) ----> 204498.....
              |Aj 8 (Tereza) ----> 497040.....
              |Aj 9 (Tereza) ----> 466200...............
              |Z 8 (Hana) ----> 1485792..............
              |D 8 (Hana) ----> 1485792..............
              |D 9 (Hana) ----> 1399080..............
              |Z 9 (Hana) ----> 1399080........
              |Aj 6 (Tereza) ----> 724080.....
              |Aj 7 (Tereza) ----> 546720....................
              |F 8 (Alena) ----> 2005728...................
              |F 9 (Alena) ----> 1894752...................
              |M 9 (Alena) ----> 1894752....................
              |M 8 (Alena) ----> 2005728.........................
              |Př 8 (Lucka) ----> 2480904.........................
              |Ch 8 (Lucka) ----> 2480904.......................
              |Př 9 (Lucka) ----> 2354304........................
              |Ch 9 (Lucka) ----> 2354304...................
              |Z 7 (Hana) ----> 1922304...................
              |Čj 7 (Hana) ----> 1922304...................................
              |D 6 (Hana) ----> 3480960...................
              |D 7 (Hana) ----> 1922304...................................
              |Z 6 (Hana) ----> 3480960...................................
              |Čj 6 (Hana) ----> 3480960........................................
              |F 6 (Alena) ----> 4014816........................................
              |M 6 (Alena) ----> 4014816.........................
              |M 7 (Alena) ----> 2465496.........................
              |F 7 (Alena) ----> 2465496......................
              |Vl 4/5 (Tereza) ----> 2256576............................................
              |Vl 4 (Tereza) ----> 4413552.........................................
              |Př 6 (Lucka) ----> 4036800..............................
              |Př 7 (Lucka) ----> 3033984..................................
              |M 3/5 (Martina) ----> 3432684...................................
              |Čj 3/5 (Martina) ----> 3432684..........................
              |Prv/Př  2/4 (Gita) ----> 2655450...........................
              |Čj 2/4 (Gita) ----> 2655450..........................
              |M 2/4 (Gita) ----> 2655450....................................................
              |Čj 9 (Bohunka) ----> 5185464...................................
              |Inf 9 (Eva) ----> 3514680..........................................
              |Čj 8 (Bohunka) ----> 4194096.............................................
              |Inf 8 (Eva) ----> 4506048...................................................................
              |Čj 2 (Gita) ----> 6693120...............................................
              |Čj 3 (Martina) ----> 4717440................................................
              |Aj 3 (Martina) ----> 4717440........................................................
              |Vl 5 (Tereza) ----> 5641440
    """.stripMargin

    parseOrder(h)
  }

  def order2 = {
    parseOrder("""F,M 6 (Alena) 4x ----> 129648.
                 |Aj,Čj 3 (Martina) 5x ----> 158916
                 |Aj,Vv 6/7 (Tereza) 3x ----> 1134
                 |Př,Vo 6/7 (Lucka) 2x ----> 10080...
                 |Čj,Prv 2 (Gita) 3x ----> 249528
                 |M,F 6/7 (Alena) 3x ----> 11346.
                 |F,M 8 (Alena) 4x ----> 68520
                 |Př,Ch 9 (Lucka) 2x ----> 64848..
                 |Čj,SPV 9 (Bohunka) 4x ----> 129960
                 |M,F 7 (Alena) 4x ----> 72240.
                 |Tv 3/5 (Lucka) 2x ----> 28350.
                 |Čj,Vv,M 3/5 (Martina) 13x ----> 103264
                 |Z,Hv 8/9 (Hana) 2x ----> 5016.
                 |D,Z 8 (Hana) 3x ----> 68520.
                 |D,Čj,Z 6 (Hana) 5x ----> 129648
                 |Rv,Pč,Vo 8/9 (Eva) 3x ----> 7506.
                 |Inf,Spv 8 (Eva) 3x ----> 101448..
                 |Prv,Př,Hv,Tv,M,Pč,Čj 2/4 (Gita) 18x ----> 204936.
                 |Z,Čj,D 7 (Hana) 5x ----> 72240
                 |Z,D 9 (Hana) 3x ----> 68520
                 |Prv,Př,Hv 3/5 (Tereza) 3x ----> 10610..
                 |Vv,Vl 4 (Tereza) 2x ----> 126348
                 |Př,Ch 8/9 (Lucka) 2x ----> 4698
                 |Čj,D,Z,Hv 6/7 (Hana) 4x ----> 11346.
                 |M,F 9 (Alena) 4x ----> 68520
                 |Vv,Aj 8/9 (Tereza) 2x ----> 684
                 |F,M 8/9 (Alena) 3x ----> 5016
                 |Rv,Př 7 (Lucka) 2x ----> 68688.
                 |Ch,Př 8 (Lucka) 2x ----> 64848
                 |Tv_Dív,Tv_Chl 6/7/8/9 (Lucka) 4x ----> 46
                 |Čj 8/9 (Bohunka) 1x ----> 10266
                 |Pč 6/7 (Eva) 1x ----> 10080
                 |Vv 2/4 (Tereza) 1x ----> 21760
                 |Aj 9 (Tereza) 2x ----> 9696.
                 |Aj 8 (Tereza) 2x ----> 9696
                 |Aj 7 (Tereza) 2x ----> 10104
                 |Aj 6 (Tereza) 2x ----> 14160
                 |Aj 4/5 (Martina) 3x ----> 61590.
                 |Pč 3/5 (Eva) 1x ----> 47992
                 |Inf 5/6 (Eva) 1x ----> 39648.
                 |Vl 4/5 (Tereza) 1x ----> 61590.
                 |Inf 9 (Eva) 2x ----> 101448.
                 |Vv 5 (Martina) 1x ----> 61590.
                 |Př 6 (Lucka) 1x ----> 119088.
                 |Prv 3 (Tereza) 1x ----> 127092.
                 |Čj 8 (Bohunka) 3x ----> 129960.
                 |Vl 5 (Tereza) 1x ----> 61590..
                 |Rj 7 (Iva) 2x ----> 208920""".stripMargin)
  }

  println(order.mkString("\n"))

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
    order(t1.id) < order(t2.id)
  }
}

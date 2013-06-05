package x

object Output {

  def printTable(cs:Seq[Seq[Any]],leftAxisDays:Boolean) {
    val leftAxisPad = if(leftAxisDays) 10 else 3

    val colLengths = cs.map(ds => ds.map(tj => if(tj==null) 1 else tj.toString.length)).foldLeft((0 to cs(0).size).map(x=>0))((lengths,dayLengths) => {
      lengths.zip(dayLengths).map(x => math.max(x._1,x._2))
    }).toList

    def divide {
      print("".padTo(leftAxisPad+4,"-").mkString)
      colLengths.foreach(cl => print("-"+"".padTo(cl,"-").mkString+"--"))
      print("\n")
    }

    def printLine(ds:Seq[Any],leftAxis:String) {
      print("| "+leftAxis.padTo(leftAxisPad," ").mkString+" | ")
      ds.zip(colLengths).foreach(x => {
        val tj: String = if (x._1==null) "" else x._1.toString
        print(tj.padTo(x._2," ").mkString+" | ")
      })
      print("\n")
    }

    divide

    printLine((0 to cs(0).size).map(_.toString).toList,"")

    divide

    cs.zipWithIndex.foreach(ds => {
      printLine(ds._1,if(leftAxisDays) DAY_NAME(ds._2) else (ds._2+1).toString+".")
      divide
    })
  }

  def printSchedule(schoolSchedule:SchoolSchedule) {

    def printClassSchedule(cs:ClassSchedule) {
      printTable(cs.classSchedule.map(x=>x.toSeq),true)
    }

    (FIRST_GRADE to LAST_GRADE).foreach(gr => {
      println((gr+1)+". Třída")
      printClassSchedule(schoolSchedule.schoolSchedule(gr))
    })

    val byDays = (MONDAY to FRIDAY).map(d => (FIRST_GRADE to LAST_GRADE).map(gr => schoolSchedule.schoolSchedule(gr).classSchedule(d)).toArray)
    byDays.zipWithIndex.foreach(daySch => {
      println(DAY_NAME(daySch._2))
      printTable(daySch._1.map(x=>x.toSeq),false)
    })

  }

}

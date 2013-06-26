package solver

object Output {

  def printCSV(cs:Seq[Seq[Any]],leftAxisDays:Boolean) {
    def printLine(ds:Seq[Any],leftAxis:String) {
      print(leftAxis+",")
      ds.foreach(x => {
        val tj: String = if (x==null) "" else x.toString
        print(tj+",")
      })
      print("\n")
    }

    cs.zipWithIndex.foreach(ds => {
      printLine(ds._1,if(leftAxisDays) DAY_NAME(ds._2) else (ds._2+1).toString+".")
    })
  }

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

  def printSchedule(schoolSchedule:SchoolSchedule, simple:Boolean = true) {

    if(!simple) {
      (FIRST_GRADE to LAST_GRADE).foreach(gr => {
        println((gr+1)+". Třída")
        printCSV(schoolSchedule.schoolSchedule(gr).classSchedule.map(x=>x.toSeq),true)
      })

      var byTeachers = Map[Teacher,SchoolSchedule]()
      (FIRST_GRADE to LAST_GRADE).foreach(gr => {
        (MONDAY to FRIDAY).foreach(d => {
          (FIRST_HOUR to LAST_HOUR).foreach(h => {
            Option(schoolSchedule.schoolSchedule(gr).classSchedule(d)(h)) match {
              case None => {}
              case Some(tj) => {
                val ss = byTeachers.get(tj.teacher) match {
                  case None => new SchoolSchedule
                  case Some(ss) => ss
                }
                tj.classHour.classes.foreach(rgr => ss.schoolSchedule(rgr).classSchedule(d)(h) = tj)
                byTeachers = byTeachers + (tj.teacher -> ss)
              }
            }
          })
        })
      })
      byTeachers.foreach(tss => {
        println(tss._1.name)
        val byDays = (MONDAY to FRIDAY).map(d => (FIRST_GRADE to LAST_GRADE).map(gr => byTeachers(tss._1).schoolSchedule(gr).classSchedule(d)).toArray)
        byDays.zipWithIndex.foreach(daySch => {
          println(DAY_NAME(daySch._2))
          printCSV(daySch._1.map(x=>x.toSeq),false)
        })
      })
    }

    val byDays = (MONDAY to FRIDAY).map(d => (FIRST_GRADE to LAST_GRADE).map(gr => schoolSchedule.schoolSchedule(gr).classSchedule(d)).toArray)
    byDays.zipWithIndex.foreach(daySch => {
        println(DAY_NAME(daySch._2))
        printCSV(daySch._1.map(x=>x.toSeq),false)
    })

  }

  def printTiles(places:Array[Array[Array[Int]]],tiles:Seq[Tile],placed:Seq[Array[Int]]) {
    def findJob(grade:Int, day:Int, hour:Int) = {
      val allThatDayAndHour = placed.filter(pi => {
        (pi(0) == day) && (pi(1) == hour)
      })
      val find = allThatDayAndHour.find(pi => {
        val tile = tiles(pi(2))
        tile.job.classHour.classes.contains(grade+1)
      })
      find.map(x => tiles(x(2)).job)
    }
    def tileToLine(tile:Array[Int],day:Int,hour:Int) = {
      def isBitThere(pos:Int,int:Int) = {
        val aux = math.pow(2,pos).toInt
        (int & aux) > 0
      }
      (FIRST_GRADE+1 to LAST_GRADE).map(gr => if(isBitThere(gr-1,tile(0))) {
        findJob(gr-1,day,hour) match {
          case None => "?"
          case Some(job) => job.toString
        }
      } else "-")
    }
    (MONDAY to FRIDAY).foreach(d => {
      println(DAY_NAME(d))
      val dayTable = (FIRST_HOUR to LAST_HOUR).map(h => {
        tileToLine(places(d)(h),d,h)
      })
      val rotatedDayTable = (FIRST_GRADE+1 to LAST_GRADE).map(gr => {
        val ngr = gr - 1
        dayTable.map(allGrs => allGrs(ngr))
      })
      printTable(rotatedDayTable,false)
    })
  }

}

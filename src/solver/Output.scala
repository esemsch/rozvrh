package solver

import java.io.{File, PrintWriter}
import spray.json._
import scala.collection.Map

object Output extends App {

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

  def printRow(row:Array[Tile]) = {
    row
      .sortWith((t1,t2) => t1.job.teacher.name.compareTo(t2.job.teacher.name)<=0)
      .map(t => t.job.toString)
      .map(s => "%23s" format s)
      .mkString(" | ")
  }

  def printDayAndHour(day: Int, hour: Int) = {
    DAY_NAME(day)+" "+hour+"."
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

    printLine((0 to cs(0).size-1).map(_.toString).toList,"")

    divide

    cs.zipWithIndex.foreach(ds => {
      printLine(ds._1,if(leftAxisDays) DAY_NAME(ds._2) else (ds._2+1).toString+".")
      divide
    })
  }

  def printSchedule(schoolSchedule:SchoolSchedule, simple:Boolean = true) {

    def printData(cs:Seq[Seq[Any]],leftAxisDays:Boolean) {
      printTable(cs,leftAxisDays)
    }

    if(!simple) {
      (FIRST_GRADE to LAST_GRADE).foreach(gr => {
        println((gr+1)+". Třída")
        printData(schoolSchedule.schoolSchedule(gr).classSchedule.map(x=>x.toSeq),true)
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
          printData(daySch._1.map(x=>x.toSeq),false)
        })
      })
    }

    val byDays = (MONDAY to FRIDAY).map(d => (FIRST_GRADE to LAST_GRADE).map(gr => schoolSchedule.schoolSchedule(gr).classSchedule(d)).toArray)
    byDays.zipWithIndex.foreach(daySch => {
        println(DAY_NAME(daySch._2))
      printData(daySch._1.map(x=>x.toSeq),false)
    })

  }

  def saveToFile(schoolSchedule:SchoolSchedule,file:String) {
    var buf = ""

    def line(ds:Seq[Any]) {
      ds.foreach(x => {
        val tj: String = if (x==null) "" else x.toString
        buf = buf + (tj+",")
      })
      buf = buf + "\n"
    }

    val byDays = (MONDAY to FRIDAY).map(d => (FIRST_GRADE to LAST_GRADE).map(gr => schoolSchedule.schoolSchedule(gr).classSchedule(d)).toArray)
    byDays.zipWithIndex.foreach(daySch => {
      daySch._1.map(x=>x.toSeq).foreach(y => line(y))
    })

    val writer = new PrintWriter(new File(file))
    writer.write(buf)
    writer.flush
    writer.close
  }

  def printTiles(solverState:SolverState) {
    def findJob(grade:Int, day:Int, hour:Int) = {
      val allThatDayAndHour = solverState.placed.filter(pi => {
        (pi(0) == day) && (pi(1) == hour)
      })
      val find = allThatDayAndHour.find(pi => {
        val tile = solverState.tiles(pi(2))
        tile.job.classHour.classes.contains(grade)
      })
      find.map(x => solverState.tiles(x(2)).job)
    }
    def tileToLine(tile:Array[Int],day:Int,hour:Int) = {
      def isBitThere(pos:Int,int:Int) = {
        val aux = math.pow(2,pos).toInt
        (int & aux) > 0
      }
      (FIRST_GRADE to LAST_GRADE).map(gr => if(isBitThere(gr,tile(0))) {
        findJob(gr,day,hour) match {
          case None => "x"
          case Some(job) => job.toString
        }
      } else "-")
    }
    (MONDAY to FRIDAY).foreach(d => {
      println(DAY_NAME(d))
      val dayTable = (FIRST_HOUR to LAST_HOUR).map(h => {
        tileToLine(solverState.places(d)(h),d,h)
      })
      val rotatedDayTable = (FIRST_GRADE to LAST_GRADE).map(gr => {
        val ngr = gr
        dayTable.map(allGrs => allGrs(ngr))
      })
      printTable(rotatedDayTable,false)
    })
  }

  object SolverStateJsonProtocol extends DefaultJsonProtocol {

    implicit val teachFormat = jsonFormat1(Teacher)

    implicit val classHourFormat = new JsonFormat[ClassHour] {
      def write(ch: ClassHour) = JsObject(
        "subject" -> JsString(ch.subject),
        "classes" -> ch.classes.toJson
      )

      def read(value:JsValue) = {
        val fields: Seq[JsValue] = value.asJsObject.getFields("subject","classes")
        ClassHour(fields(0).toString(),fields(1).convertTo[Set[Int]])
      }
    }

    implicit val teachersJobFormat = jsonFormat2(TeachersJob)

    implicit val jobFormat = jsonFormat3(Job)

    implicit val tileFormat = jsonFormat4(Tile)

    implicit val solverStateFormat = new RootJsonFormat[SolverState] {
      def write(ss: SolverState) = JsObject(
        "tiles" -> ss.tiles.toJson,
        "places" -> ss.places.toJson,
        "counts" -> ss.counts.toJson,
        "placed" -> ss.placed.toJson,
        "mapToPlaced" -> ss.mapToPlaced.toJson,
        "tilesPerDay" -> ss.tilesPerDay.toJson,
        "jobs" -> ss.jobs._1.toJson,
        "jobsLookup" -> JsArray(ss.jobs._2.map(e => JsObject(e._1.toString -> e._2.toJson)).toList),
        "tilesLookup" -> JsObject(ss.tilesLookup.map(e => e._1 -> JsArray(e._2
          .map(f => JsObject("key" -> f._1.toJson,"value" -> f._2.toJson))
          .toList))),
        "teachers" -> ss.teachers.toJson
      )
      def read(value: JsValue) = {
        val fields: Seq[JsValue] = value.asJsObject.getFields("tiles","places","counts","placed","mapToPlaced","tilesPerDay","jobs","jobsLookup","tilesLookup","teachers")
        val tiles = fields(0).convertTo[Array[Tile]]
        val places = fields(1).convertTo[Array[Array[Array[Int]]]]
        val counts = fields(2).convertTo[Array[Int]]
        val placed = fields(3).convertTo[Array[Array[Int]]]
        val mapToPlaced = fields(4).convertTo[Array[Int]]
        val tilesPerDay = fields(5).convertTo[Array[(Array[Int], Tile)]]
        val jobs = fields(6).convertTo[List[Job]]
        val jobsLookup = fields(7).convertTo[List[JsObject]]

        SolverState(
          new TilesSolver(tiles,places,counts,placed,mapToPlaced,tilesPerDay),
          tiles,
          places,
          counts,
          placed,
          mapToPlaced,
          tilesPerDay,
          null,
          null,
          null
        )
      }
    }
  }

  def saveSolverState(file:String,solverState: SolverState) {
    val pw = new PrintWriter(new File(file))

    import SolverStateJsonProtocol._

    pw.write(solverState.toJson.prettyPrint)

    pw.flush()
    pw.close()
  }

  val solverState = TilesSolver.factory

  HardConstraints.applyHardConstraints(solverState)

  Output.saveSolverState("solver-state.json",solverState)
}

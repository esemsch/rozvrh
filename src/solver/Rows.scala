package solver

import collection.mutable

object Rows extends App {

  val (tilesSolver,tiles,places,counts,placed,mapToPlaced,tilesPerDay,jobs,tilesLookup,teachers) = TilesSolver.factory

  List(Teacher("Iva") -> (Set(MONDAY,THURSDAY,FRIDAY),0 to 7),
    Teacher("Iva") -> (Set(TUESDAY),(0 to 1)++(4 to 7)),
    Teacher("Iva") -> (Set(WEDNESDAY),(0 to 1)++(4 to 7)),
    Teacher("Bohunka") -> (Set(TUESDAY,WEDNESDAY,THURSDAY),0 to 7),
    Teacher("Eva") -> (Set(TUESDAY,FRIDAY),0 to 7),
    Teacher("Eva") -> (Set(MONDAY,WEDNESDAY,THURSDAY),0 to 2),
    Teacher("Lucka") -> (Set(FRIDAY),0 to 7),
    Teacher("Tereza") -> (Set(THURSDAY),5 to 7),
    Teacher("Gita") -> (Set(THURSDAY),3 to 7),
    Teacher("Hana") -> (Set(TUESDAY,WEDNESDAY,FRIDAY),5 to 7)).foreach(ta => {
    val index = teachers.indexOf(ta._1)
    ta._2._1.foreach(d => {
      ta._2._2.foreach(h => places(d)(h)(1) = setBit(places(d)(h)(1),index))
    })
  })

  val daysOrder = Array(
    THURSDAY,TUESDAY,WEDNESDAY,MONDAY,FRIDAY,
    THURSDAY,TUESDAY,WEDNESDAY,MONDAY,FRIDAY,
    THURSDAY,TUESDAY,WEDNESDAY,MONDAY,FRIDAY,
    THURSDAY,TUESDAY,WEDNESDAY,MONDAY,FRIDAY,
    1000)

  def freeHours(days:Seq[Int],grades:Seq[Int],hours:Seq[Int]) {
    days.foreach(d => hours.foreach(h => {
      grades.foreach(gr => {
        places(d)(h)(0) = setBit(places(d)(h)(0),(gr-1))
      })
    }))
  }

  freeHours(MONDAY to FRIDAY,List(FIRST_GRADE+1),List(0))
  freeHours(MONDAY to FRIDAY,List(FIRST_GRADE+2),List(0))
//  freeHours((TUESDAY to FRIDAY),List(FIRST_GRADE+2),List(0))
  freeHours((MONDAY to TUESDAY)++(THURSDAY to FRIDAY),FIRST_GRADE+3 to FIRST_GRADE+4,List(0))
  freeHours(MONDAY to FRIDAY,FIRST_GRADE+1 to FIRST_GRADE+2,6 to 7)
  freeHours(MONDAY to FRIDAY,FIRST_GRADE+3 to FIRST_GRADE+4,List(6))
  freeHours(TUESDAY to FRIDAY,FIRST_GRADE+3 to FIRST_GRADE+4,6 to 7)
  freeHours(List(TUESDAY,WEDNESDAY,FRIDAY),FIRST_GRADE+1 to LAST_GRADE,6 to 7)

  tilesSolver.applyTile(tilesLookup("Hana")((Set(5,6,7,8),false)),MONDAY,5)
  tilesSolver.applyTile(tilesLookup("Lucka")((Set(5,6,7,8),false)),MONDAY,6)
  tilesSolver.applyTile(tilesLookup("Lucka")((Set(5,6,7,8),false)),THURSDAY,5)
  tilesSolver.applyTile(tilesLookup("Hana")((Set(5,6,7,8),false)),THURSDAY,6)

//  tilesSolver.applyTile(tilesLookup("Bohunka")((Set(7),true)),FRIDAY,5)

//  tilesSolver.applyTile(tilesLookup("Tereza")((Set(7,8),false)),FRIDAY,0)
  tilesSolver.applyTile(tilesLookup("Tereza")((Set(3,4),false)),WEDNESDAY,5)
  tilesSolver.applyTile(tilesLookup("Tereza")((Set(5,6),false)),FRIDAY,5)
//  tilesSolver.applyTile(tilesLookup("Hana")(Set(7,8)),FRIDAY,5)
//  tilesSolver.applyTile(tilesLookup("Hana")(Set(7,8)),WEDNESDAY,0)
//  tilesSolver.applyTile(tilesLookup("Hana")(Set(6)),WEDNESDAY,5)
//  tilesSolver.applyTile(tilesLookup("Lucka")(Set(5,6),false),WEDNESDAY,5)
//  tilesSolver.applyTile(tilesLookup("Lucka")(Set(7,8),false),TUESDAY,0)
//  tilesSolver.applyTile(tilesLookup("Lucka")(Set(7,8)),WEDNESDAY,5)
  tilesSolver.applyTile(tilesLookup("Alena")((Set(8),false)),WEDNESDAY,5)
  tilesSolver.applyTile(tilesLookup("Eva")((Set(7,8),false)),MONDAY,7)
  tilesSolver.applyTile(tilesLookup("Eva")((Set(7,8),false)),THURSDAY,7)
//  tilesSolver.applyTile(tilesLookup("Eva")((Set(7,8),false)),WEDNESDAY,5)

  val teachersOrder = List(
    Set("Iva","Bohunka"),
    Set("Eva","Hana","Lucka"),
    Set("Tereza","Alena","Gita","Martina")
  )

  val rows = H.tileIndexRows.sortBy(r => {
    r.map(ti => {
      val tile = tiles(ti)
      teachersOrder.indexOf(teachersOrder.find(x => x.exists(n => n == tile.job.teacher.name)))
    }).foldLeft(0)((sum,x) => sum - math.pow(2,10-x).toInt)
  })

  class RowOpen {
    var open = new mutable.HashSet[Int]() {
      (1 to rows.size).foreach(t => add(t-1))
    }

    val stack = new mutable.Stack[Set[Int]]()

    def popFromOpen(ind:Int) {
      val neverApplicable = open.filter(!rowEverApplicable(_)).toSet
      stack.push(neverApplicable)
      open --= neverApplicable
    }
    def pushToOpen(ind:Int) {
      open ++= stack.pop()
    }
    def options(day:Int,hour:Int):List[Int] = {
      open.filter(ri => rowApplicable(ri,day,hour)).toList.sortBy(ri => calcConstraints(day,hour,ri))
    }
    def isEmpty:Boolean = open.isEmpty
  }

  val rowOpen = new RowOpen()

  def filterRows(day:Int,hour:Int,subjs:List[(String,Set[String],(Set[Int],Boolean))]):(Int,Int,List[Int]) = {
    val options = rowOpen.options(day, hour)
    val ret = options.filter(ri => {
      subjs.forall(x => {
        rows(ri).exists(ti => {
          val t = tiles(ti)
          (x._1 == null || t.job.teacher.name == x._1) &&
          (x._2 == null || (t.job.classHour.subjects diff x._2).size < t.job.classHour.subjects.size) &&
          (x._3 == null || (x._3._1 == t.job.classHour.classes)) &&
          (x._3 == null || (x._3._2 == t.job.classHour.mainSubject))
        })
      })
    })

//    ret.foreach(ri => {
//      val r = rows(ri)
//      println(r.map(ti => tiles(ti).job).mkString(","))
//    })
//
//    println("===============================================")
    (day,hour,ret)
  }
  def filterRows(day:Int,hour:Int,teacher:String):(Int,Int,List[Int]) = {
    filterRows(day,hour,List[(String,Set[String],(Set[Int],Boolean))]((teacher,null,null)))
  }
  def filterRows(day:Int,hour:Int,teacher:String,subject:String,grade:Int,main:Boolean):(Int,Int,List[Int]) = {
    filterRows(day,hour,List[(String,Set[String],(Set[Int],Boolean))]((teacher,Set[String](subject),(Set(grade),main))))
  }
  def filterRows(day:Int,hour:Int,teacher:String,subject:String,grades:Set[Int],main:Boolean):(Int,Int,List[Int]) = {
    filterRows(day,hour,List[(String,Set[String],(Set[Int],Boolean))]((teacher,if(subject!=null)Set(subject) else null,(grades,main))))
  }
  def preassignRow(candidateRows:(Int,Int,List[Int])) {
    val ri = candidateRows._3.head
    applyRow(ri,candidateRows._1,candidateRows._2)
    rowOpen.popFromOpen(ri)
  }

  preassignRow(filterRows(WEDNESDAY,4,"Tereza",null,Set(3,4),false))
//  preassignRow(filterRows(TUESDAY,4,List(("Tereza",null,(Set(5,6),false)),("Lucka",null,null))))
//  preassignRow(filterRows(MONDAY,4,"Eva",null,Set(8)))
  preassignRow(filterRows(THURSDAY,4,"Eva",null,Set(7,8),false))
//  preassignRow(filterRows(MONDAY,1,"Bohunka",null,Set(7,8)))
//  preassignRow(filterRows(MONDAY,2,"Bohunka",null,Set(8)))
//  preassignRow(filterRows(MONDAY,3,"Bohunka",null,Set(7)))
  preassignRow(filterRows(MONDAY,1,"Hana",null,Set(5,6),false))

  preassignRow(filterRows(TUESDAY,2,"Iva",null,Set(6),false))
  preassignRow(filterRows(TUESDAY,3,"Iva",null,Set(7),false))

  preassignRow(filterRows(WEDNESDAY,2,"Iva",null,Set(6),false))
  preassignRow(filterRows(WEDNESDAY,3,List(("Iva",null,(Set(7),false)),("Eva",null,(Set(8),false)),("Lucka",null,null))))

  preassignRow(filterRows(FRIDAY,4,"Tereza",null,Set(5,6),false))
//  preassignRow(filterRows(FRIDAY,1,"Bohunka",null,Set(7)))
//  preassignRow(filterRows(FRIDAY,2,"Bohunka",null,Set(8)))
//  preassignRow(filterRows(FRIDAY,3,"Bohunka",null,Set(7)))
//  preassignRow(filterRows(FRIDAY,4,"Bohunka",null,Set(8)))


//  preassignRow(filterRows(MONDAY,3,List(("Alena",null,Set(5,6)),("Hana",null,Set(7)))))

//  preassignRow(filterRows(MONDAY,1,"Tereza",null,Set(5,6)))
//  preassignRow(filterRows(TUESDAY,2,"Martina","Vv",null))
//  preassignRow(filterRows(TUESDAY,3,"Martina","Vv",Set(4)))
//  preassignRow(filterRows(MONDAY,4,null,"Vv",Set(3)))
//  preassignRow(filterRows(THURSDAY,2,"Alena"))
//  preassignRow(filterRows(THURSDAY,3,"Alena"))
//  preassignRow(filterRows(FRIDAY,4,List(("Tereza",Set("Vv"),Set(5,6)),("Alena",null,null))))

  def rowApplicable(rowInd:Int,day:Int,hour:Int) = {
    rows(rowInd).forall(ti => counts(ti)>0 && tilesSolver.applicable(tiles(ti),day,hour))
  }

  def rowEverApplicable(rowInd:Int) = {
    rows(rowInd).forall(ti => counts(ti)>0)
  }

  def applyRow(rowInd:Int,day:Int,hour:Int) {
    rows(rowInd).foreach(ti => {
      tilesSolver.applyTile(tiles(ti),day,hour)
    })
  }

  def revertRow(rowInd:Int,day:Int,hour:Int) {
    rows(rowInd).foreach(ti => tilesSolver.revertTile(tiles(ti),day,hour))
  }

  def calcConstraints(day:Int,hour:Int,rowInd:Int) = {
    val row = rows(rowInd)
    val rowTiles = row.map(ti => tiles(ti))
    def teachers = {
      rowTiles.foldLeft(0.0)((tot,t) => {
        val teacherInd = teachersOrder.indexOf(teachersOrder.find(x => x.exists(n => n == t.job.teacher.name)))
        val to = if(teacherInd>=0) (teacherInd-teachersOrder.size).toDouble/teachersOrder.size else 0
        tot + to
      })
    }
    def teachersSpread = {
      val tilesPerThisDay = tilesPerDay.filter(td => td._1(day)>0 && !teachersOrder.contains(td._2.job.teacher.name)).map(td => (td._2,td._1(day)))
      rowTiles.foldLeft(0.0)((sum,t) => {
        val sumT = tilesPerThisDay.filter(x => x._1.teacher == t.teacher).foldLeft(0)((a,b) => a + b._2)
        sum + sumT
      })/rowTiles.size
    }
    def spread = {
      val tilesPerThisDay = tilesPerDay.filter(td => td._1(day)>0)
      rowTiles.foldLeft(0)((tot,t) => {
        tot + (if(tilesPerThisDay.exists(_._2==t)) 1 else 0)
      })
    }
    def combined = {
      val coeff = if(hour>=1 && hour <=2) -1 else 1
      rowTiles.foldLeft(0)((tot,t) => tot + (if(t.job.classHour.combinedClasses) coeff else 0))
    }
    def main = {
      val coeff = if(hour>=1 && hour <=4) -1 else 1
      rowTiles.foldLeft(0)((tot,t) => tot + (if(t.job.classHour.mainSubject) coeff*t.job.classHour.classes.size else 0))
    }
//        println(rowTiles.map(t=>t.job).mkString(",")+" --- Teachers = "+teachers+" Spread = "+spread+" Combined = "+combined+" Main = "+main+" Teachers spread = "+teachersSpread)
//    spread
    teachers + spread + combined + main + teachersSpread
//    teachers + spread + combined + main + teachersSpread
  }

  var cnt = 0
  def search(dayIndex:Int,hour:Int):Boolean = {
    val day = daysOrder(dayIndex)
    cnt = cnt + 1
    if(cnt%10000==0) {
      println(cnt)
    }
    if(cnt%100000==0) {
      Output.printTiles(places,tiles,placed)
      println(Data.data2.foldLeft(0)((total,j) => total + j.count)-placed.filter(pl => (pl(2) != -1)).size)
      tiles.filter(t => counts(t.id)>0).sortWith((t1,t2) => t1.job.teacher.name < t2.job.teacher.name).foreach(t => println(t.job+" --- "+counts(t.id)))
    }
    if(day>FRIDAY) {
      true
    }
    else if(hour<=7 && tilesSolver.hourComplete(day,hour)) {
      search((dayIndex+1),if(day==daysOrder(4)) (hour+1) else hour)
    }
//    else if(((day==TUESDAY || day==WEDNESDAY) && hour>=6)||((day != TUESDAY && day != WEDNESDAY) && hour>=5)) {
//    else if(((day==TUESDAY || day==WEDNESDAY || day==FRIDAY) && hour>=6)||((day != TUESDAY && day != WEDNESDAY && day!=FRIDAY) && hour>=5)) {
//    else if(((day==TUESDAY) && hour>=6)||((day != TUESDAY) && hour>=5)) {
//    else if(hour>=5) {
//      search((dayIndex+1),1)
//    }
    else if(rowOpen.isEmpty) {
      false
    }
    else {
      rowOpen.options(day,hour).exists(rowInd => {
        applyRow(rowInd,day,hour)
        rowOpen.popFromOpen(rowInd)
//        Output.printTiles(places,tiles,placed)
//        Console.readLine()
        if(!search(dayIndex+1,if(day==daysOrder(4)) (hour+1) else hour)) {
          revertRow(rowInd,day,hour)
          rowOpen.pushToOpen(rowInd)
          false
        } else true
      })
    }
  }

  search(0,1)

  Output.printTiles(places,tiles,placed)

  println(Data.data2.foldLeft(0)((total,j) => total + j.count)-placed.filter(pl => (pl(2) != -1)).size)
  tiles.filter(t => counts(t.id)>0).foreach(t => println(t.job+" --- "+counts(t.id)))

  def maxRowSearch(depth:Int,maxDepth:Int):Boolean = {
    val opts = (MONDAY to FRIDAY).flatMap(d => (FIRST_HOUR to LAST_HOUR).map(h => (d,h))).map(x => (tilesSolver.findMaxRow(x._1,x._2),x._1,x._2))
    .filter(x => !x._1.isEmpty).sortBy(x => {
      if(x._1.isEmpty) 0 else -x._1.foldLeft(0)((tot,ts) => tot + ts.foldLeft(0)((subTot,t) => subTot + t.job.classHour.classes.sum))
    })

    opts.exists(opt => {
      opt._1.toList.sortBy(x => -x.foldLeft(0)((subTot,t) => subTot + t.job.classHour.classes.sum)).exists(o => {
        o.foreach(t => tilesSolver.applyTile(t,opt._2,opt._3))
        if(depth==maxDepth) {
          if(tilesSolver.solve) true
          else {
            o.foreach(t => tilesSolver.revertTile(t,opt._2,opt._3))
            false
          }
        } else {
          if(maxRowSearch((depth+1),maxDepth)) true
          else {
            o.foreach(t => tilesSolver.revertTile(t,opt._2,opt._3))
            false
          }
        }
      })
    })
  }

  maxRowSearch(0,1)

  Output.printSchedule(Conversions.remapGrades(Conversions.tilesToSchoolSchedule(places,tiles,placed)))

  val schoolSchedule = Straightener.straighten2(Conversions.tilesToJobsArray(places,tiles,placed),jobs._2.toMap)

//  val preStraightenings = Map[Job,List[(Int,Int,String)]](
//    (tilesLookup("Gita")(Set(3,4)).job) -> List((MONDAY,7,"Hv"),(WEDNESDAY,2,"M"),(THURSDAY,2,"M"),(TUESDAY,1,"M")),
//    (tilesLookup("Hana")(Set(5,6)).job) -> List((THURSDAY,1,"D")),
//    (tilesLookup("Hana")(Set(5)).job) -> List((FRIDAY,1,"Z"),(MONDAY,1,"D")),
//    (tilesLookup("Hana")(Set(6)).job) -> List((FRIDAY,0,"Z"),(MONDAY,0,"D")),
//    (tilesLookup("Hana")(Set(7,8)).job) -> List((FRIDAY,5,"D")),
//    (tilesLookup("Hana")(Set(7)).job) -> List((FRIDAY,4,"Z")),
//    (tilesLookup("Hana")(Set(8)).job) -> List((FRIDAY,0,"Z"),(MONDAY,0,"D"))
//  )
//  val schoolSchedule = Straightener.straighten2(Conversions.tilesToJobsArray(places,tiles,placed),jobs._2.toMap, preStraightenings)

  Output.saveToFile(schoolSchedule,"schedule.txt")
  Output.printSchedule(Conversions.remapGrades(schoolSchedule),true)

  Checker.check(schoolSchedule,jobs._2.flatMap(x => x._2).toList)
}

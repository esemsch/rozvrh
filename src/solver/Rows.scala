package solver

import collection.mutable

object Rows extends App {

  val (tilesSolver,tiles,places,counts,placed,mapToPlaced,tilesPerDay,jobs,tilesLookup,teachers) = TilesSolver.factory

  // UNAVAILABILITY
  List(
    Teacher("Iva")     -> (Set(MONDAY,THURSDAY,FRIDAY),     0 to 7),
    Teacher("Iva")     -> (Set(TUESDAY),                   (0 to 1)++(4 to 7)),
    Teacher("Iva")     -> (Set(WEDNESDAY),                 (0 to 1)++(4 to 7)),
    Teacher("Bohunka") -> (Set(TUESDAY,WEDNESDAY,THURSDAY), 0 to 7),
    Teacher("Dana")    -> (Set(TUESDAY),                    0 to 7),
    Teacher("Lucka")   -> (Set(THURSDAY,FRIDAY),            0 to 7),
    Teacher("Tereza")  -> (Set(MONDAY,WEDNESDAY,FRIDAY),    3 to 7),
    Teacher("Tereza")  -> (Set(TUESDAY),                    3 to 6),
    Teacher("Tereza")  -> (Set(THURSDAY),                   0 to 1),
    Teacher("Gita")    -> ((MONDAY to FRIDAY).toSet,        6 to 7), // relax if needed
    Teacher("Hana")    -> (Set(TUESDAY,WEDNESDAY,FRIDAY),   5 to 7)).foreach(ta => {
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

  val teachersOrder = List(
    Set("Iva","Bohunka","Hana","Tereza","Lucka","Dana","Gita","Martina")
  )

  // FREEHOURS
  def freeHours(days:Seq[Int],grades:Seq[Int],hours:Seq[Int]) {
    days.foreach(d => hours.foreach(h => {
      grades.foreach(gr => {
        places(d)(h)(0) = setBit(places(d)(h)(0),(gr))
      })
    }))
  }

  freeHours(MONDAY to FRIDAY,                         FIRST_GRADE to FIRST_GRADE+2,    List(0))
  freeHours(MONDAY to FRIDAY,                         FIRST_GRADE to FIRST_GRADE+2,    6 to 7)
  freeHours(List(FRIDAY),                             List(FIRST_GRADE+1),             5 to 7)
  freeHours((MONDAY to TUESDAY)++(THURSDAY to FRIDAY),FIRST_GRADE+3 to FIRST_GRADE+4,  List(0))
  freeHours(MONDAY to FRIDAY,                         FIRST_GRADE+3 to FIRST_GRADE+4,  List(6))
  freeHours(TUESDAY to FRIDAY,                        FIRST_GRADE+3 to FIRST_GRADE+4,  6 to 7)
  freeHours(List(TUESDAY,WEDNESDAY,FRIDAY),           FIRST_GRADE to LAST_GRADE-2,     6 to 7)
  freeHours(List(WEDNESDAY,FRIDAY),                   List(LAST_GRADE-1,LAST_GRADE),   6 to 7)
  freeHours(List(TUESDAY),                            List(LAST_GRADE-1,LAST_GRADE),   List(5))
  freeHours(List(MONDAY),                             FIRST_GRADE+5 to LAST_GRADE,     List(0))

  // PRE-PLACEMENTS
  tilesSolver.applyTile(tilesLookup("Hana")    ((Set(5,6,7,8),  false)), MONDAY,    5)
  tilesSolver.applyTile(tilesLookup("Hana")    ((Set(5,6,7,8),  false)), MONDAY,    6)
  tilesSolver.applyTile(tilesLookup("Hana")    ((Set(5,6,7,8),  false)), THURSDAY,  5)
  tilesSolver.applyTile(tilesLookup("Hana")    ((Set(5,6,7,8),  false)), THURSDAY,  6)
  tilesSolver.applyTile(tilesLookup("Hana")    ((Set(5,6),      false)), FRIDAY,    5)

  tilesSolver.applyTile(tilesLookup("Martina") ((Set(2,4),      true)),  MONDAY,    5)
  tilesSolver.applyTile(tilesLookup("Martina") ((Set(3),        true)),  WEDNESDAY, 0)
  tilesSolver.applyTile(tilesLookup("Martina") ((Set(2,4),      true)),  FRIDAY,    5)
  tilesSolver.applyTile(tilesLookup("Martina") ((Set(3),        true)),  WEDNESDAY, 5)
  tilesSolver.applyTile(tilesLookup("Martina") ((Set(1),        true)),  TUESDAY,   5)
  tilesSolver.applyTile(tilesLookup("Martina") ((Set(1),        true)),  THURSDAY,  5)

  tilesSolver.applyTile(tilesLookup("Tereza")  ((Set(7,8),      false)), TUESDAY,   7)

  tilesSolver.applyTile(tilesLookup("Lucka")   ((Set(7,8),      false)), TUESDAY,   6)
  tilesSolver.applyTile(tilesLookup("Lucka")   ((Set(2,4),      false)), WEDNESDAY, 5)

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

  def filterRows(day:Int,hour:Int,subjs:List[(String,Set[Int])]):(Int,Int,List[Int]) = {
    val options = rowOpen.options(day, hour)
    val ret = options.filter(ri => {
      subjs.forall(x => {
        rows(ri).exists(ti => {
          val t = tiles(ti)
          (x._1 == null || t.job.teacher.name == x._1) &&
          (x._2 == null || (x._2 == t.job.classHour.classes))
        })
      })
    })

    (day,hour,ret)
  }
  def filterRows(day:Int,hour:Int,teacher:String):(Int,Int,List[Int]) = {
    filterRows(day,hour,List[(String,Set[Int])]((teacher,null)))
  }
  def filterRows(day:Int,hour:Int,teacher:String,grades:Set[Int]):(Int,Int,List[Int]) = {
    filterRows(day,hour,List[(String,Set[Int])]((teacher,grades)))
  }
  def preassignRow(candidateRows:(Int,Int,List[Int])) {
    val ri = candidateRows._3.head
    applyRow(ri,candidateRows._1,candidateRows._2)
    rowOpen.popFromOpen(ri)
  }

  // PRE-ASSIGNMENTS
  preassignRow(filterRows(TUESDAY,2,List(
    ("Iva",Set(6,7)),
    ("Lucka",Set(8))
  )))
  preassignRow(filterRows(TUESDAY,3,List(
    ("Iva",Set(8)),
    ("Lucka",Set(7))
  )))

  preassignRow(filterRows(WEDNESDAY,2,"Iva",Set(6,7)))
  preassignRow(filterRows(WEDNESDAY,3,List(
    ("Iva",Set(8)),
    ("Lucka",Set(7))
  )))

  preassignRow(filterRows(FRIDAY,4,List(
    ("Hana",Set(2,3,4)),
    ("Bohunka",Set(6,7,8))
  )))

  Stats.printStats(tiles,rows,jobs._1);

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
    teachers + spread + combined + main + teachersSpread
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
    else if(rowOpen.isEmpty) {
      false
    }
    else {
      rowOpen.options(day,hour).exists(rowInd => {
        applyRow(rowInd,day,hour)
        rowOpen.popFromOpen(rowInd)
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

  Output.printSchedule(Conversions.tilesToSchoolSchedule(places,tiles,placed))

  val schoolSchedule = Straightener.straighten2(Conversions.tilesToJobsArray(places,tiles,placed),jobs._2.toMap)

  Output.saveToFile(schoolSchedule,"schedule.txt")
  Output.printSchedule(schoolSchedule,true)

  Checker.check(schoolSchedule,jobs._2.flatMap(x => x._2).toList)
}

package x

import collection.mutable

object Rows extends App {
  val teachers = Data.data._2.toList
  val tiles = Data.data3.filter(j => true).map(j => {
    val clss = j.classHour.classes.foldLeft(0)((result,cls) => setBit(result,cls-1))
    val teacher = setBit(0,teachers.indexOf(j.teacher))
    Tile(clss,teacher,-1,j)
  }).zipWithIndex.map(ti => Tile(ti._1.classes,ti._1.teacher,ti._2,ti._1.job)).toArray
  val tilesLookup = tiles.foldLeft(Map[String,Map[Set[Int],Tile]]())((map,tile) => {
    map.get(tile.job.teacher.name) match {
      case None => {
        map + (tile.job.teacher.name -> Map(tile.job.classHour.classes -> tile))
      }
      case Some(clsToTile) => {
        map + (tile.job.teacher.name -> (clsToTile + (tile.job.classHour.classes -> tile)))
      }
    }
  })
  val places = (MONDAY to FRIDAY).map(d => (FIRST_HOUR to LAST_HOUR).map(h => Array(0,0)).toArray).toArray
  val counts = tiles.map(t => t.job.count).toArray
  val placed = tiles.flatMap(t => (1 to t.job.count).map(x => Array(-1, -1, -1))).toArray
  val mapToPlaced  = tiles.foldLeft(new Array[Int](tiles.size))((arr,t) => {
    arr(t.id) = if (t.id==0) 0 else (arr(t.id-1)+counts(t.id-1))
    arr
  })
  val tilesPerDay = tiles.map(t => (Array(0,0,0,0,0),t)).toArray
  List(Teacher("Iva") -> (Set(MONDAY,TUESDAY,WEDNESDAY,THURSDAY),0 to 7),
    Teacher("Iva") -> (Set(FRIDAY),(0 to 1)++(4 to 7)),
    Teacher("Bohunka") -> (Set(TUESDAY,WEDNESDAY,THURSDAY),0 to 7),
    Teacher("Eva") -> (Set(TUESDAY,FRIDAY),0 to 7),
    Teacher("Lucka") -> (Set(FRIDAY),0 to 7),
    Teacher("Hana") -> ((MONDAY to FRIDAY).toSet,5 to 7)).foreach(ta => {
    val index = teachers.indexOf(ta._1)
    ta._2._1.foreach(d => {
      ta._2._2.foreach(h => places(d)(h)(1) = setBit(places(d)(h)(1),index))
    })
  })
//  val daysOrder = Array(MONDAY,TUESDAY,WEDNESDAY,THURSDAY,FRIDAY,1000)
  val daysOrder = Array(FRIDAY,MONDAY,TUESDAY,WEDNESDAY,THURSDAY,1000)
  def freeHours(days:Seq[Int],grades:Seq[Int],hours:Seq[Int]) {
    days.foreach(d => hours.foreach(h => {
      grades.foreach(gr => {
        places(d)(h)(0) = setBit(places(d)(h)(0),(gr-1))
      })
    }))
  }

  freeHours(MONDAY to FRIDAY,List(FIRST_GRADE+1),List(0))
  freeHours(List(MONDAY)++(WEDNESDAY to FRIDAY),List(FIRST_GRADE+2),List(0))
  freeHours(TUESDAY to FRIDAY,FIRST_GRADE+3 to FIRST_GRADE+4,List(0))
  freeHours(MONDAY to FRIDAY,FIRST_GRADE+1 to FIRST_GRADE+2,6 to 7)
  freeHours(MONDAY to FRIDAY,FIRST_GRADE+3 to FIRST_GRADE+4,List(6))
  freeHours(TUESDAY to FRIDAY,FIRST_GRADE+3 to FIRST_GRADE+4,6 to 7)
  freeHours(List(TUESDAY,WEDNESDAY,FRIDAY),FIRST_GRADE+1 to LAST_GRADE,6 to 7)

  val tilesSolver = new TilesSolver(tiles,places,counts,placed,mapToPlaced,tilesPerDay)

  tilesSolver.applyTile(tilesLookup("Lucka")(Set(5,6,7,8)),MONDAY,5)
  tilesSolver.applyTile(tilesLookup("Lucka")(Set(5,6,7,8)),MONDAY,6)
  tilesSolver.applyTile(tilesLookup("Lucka")(Set(5,6,7,8)),THURSDAY,5)
  tilesSolver.applyTile(tilesLookup("Lucka")(Set(5,6,7,8)),THURSDAY,6)

  val teachersOrder = List("Iva","Bohunka","Eva","Hana","Lucka")

  val rows = H.tileIndexRows.sortBy(r => {
    r.map(ti => {
      val tile = tiles(ti)
      teachersOrder.indexOf(tile.job.teacher.name)
    }).foldLeft(0)((sum,x) => sum - math.pow(2,10-x).toInt)
  })

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

  def calcConstraints(day:Int,hour:Int,row:Array[Int]) = {
    val rowTiles = row.map(ti => tiles(ti))
    def teachers = {
      rowTiles.foldLeft(0)((tot,t) => {
        val teacherInd = teachersOrder.indexOf(t.job.teacher.name)
        val to = if(teacherInd>=0) (teacherInd-teachersOrder.size) else 0
        tot + to
      })
    }
    def spread = {
      val tilesPerThisDay = tilesPerDay.filter(td => td._1(day)>0)
      rowTiles.foldLeft(0)((tot,t) => {
        tot + (if(tilesPerThisDay.exists(_._2==t)) 1 else -1)
      })
    }
    def combined = {
      if(hour>=1 && hour <=3) {
        rowTiles.foldLeft(0)((tot,t) => tot + (if(t.job.classHour.combinedClasses) 1 else -1))
      } else {
        rowTiles.foldLeft(0)((tot,t) => tot + (if(t.job.classHour.combinedClasses) 1 else -1))
      }
    }
    def main = {
      if(hour>=1 && hour <=4) {
        rowTiles.foldLeft(0)((tot,t) => tot + (if(t.job.classHour.mainSubject) -1 else 1))
      } else {
        rowTiles.foldLeft(0)((tot,t) => tot + (if(t.job.classHour.mainSubject) 1 else -1))
      }
    }
//    println(rowTiles.map(t=>t.job).mkString(",")+" --- Teachers = "+teachers+" Spread = "+spread+" Combined = "+combined+" Main = "+main)
    teachers + spread + combined + main
  }

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
      open.filter(ri => rowApplicable(ri,day,hour)).toList.sortBy(ri => calcConstraints(day,hour,rows(ri)))
    }
    def isEmpty:Boolean = open.isEmpty
  }

  val rowOpen = new RowOpen()

  def preassignRow(day:Int,hour:Int,teacher:String) {
    val options = rowOpen.options(day,hour).map(ri => (rows(ri).map(ti => tiles(ti)),calcConstraints(day,hour,rows(ri))))
//    println(options.map(o=>o._1.map(t=>t.job).mkString(",")+" --- "+o._2).mkString("\n"))
    val ri = rowOpen.options(day,hour).filter(ri => rows(ri).exists(ti => tiles(ti).job.teacher.name == teacher)).head
    applyRow(ri,day,hour)
    rowOpen.popFromOpen(ri)
  }
  preassignRow(FRIDAY,2,"Iva")
  preassignRow(FRIDAY,3,"Iva")

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
      search(dayIndex,(hour+1))
    }
    else if(((day==TUESDAY || day==WEDNESDAY) && hour>=6)||((day != TUESDAY && day != WEDNESDAY) && hour>=5)) {
//    else if(((day==TUESDAY || day==WEDNESDAY || day==FRIDAY) && hour>=6)||((day != TUESDAY && day != WEDNESDAY && day!=FRIDAY) && hour>=5)) {
//    else if(((day==TUESDAY) && hour>=6)||((day != TUESDAY) && hour>=5)) {
//    else if(hour>=5) {
      search((dayIndex+1),1)
    }
    else if(rowOpen.isEmpty) {
      false
    }
    else {
      rowOpen.options(day,hour).exists(rowInd => {
        applyRow(rowInd,day,hour)
        rowOpen.popFromOpen(rowInd)
        if(!search(dayIndex,(hour+1))) {
          revertRow(rowInd,day,hour)
          rowOpen.pushToOpen(rowInd)
          false
        } else true
      })
    }
  }

  search(0,1)

  Output.printTiles(places,tiles,placed)

  //  println(open.open.map(t => t.job + " ---- "+counts(t.id)).mkString("\n"))

  println(Data.data2.foldLeft(0)((total,j) => total + j.count)-placed.filter(pl => (pl(2) != -1)).size)
  tiles.filter(t => counts(t.id)>0).foreach(t => println(t.job+" --- "+counts(t.id)))

  tilesSolver.calcRows(0,0,Set(2,3,4,5,6,7,8)).foreach(r => {
    println(r.map(ri => tiles(ri).job).mkString(","))
  })
  println("--------------------------")
  tilesSolver.calcRows(0,7,Set(5,6,7,8)).foreach(r => {
    println(r.map(ri => tiles(ri).job).mkString(","))
  })
  println("--------------------------")
  tilesSolver.calcRows(4,5,Set(1,2,3,4,5,6,7,8)).foreach(r => {
    println(r.map(ri => tiles(ri).job).mkString(","))
  })

  tilesSolver.solve
}

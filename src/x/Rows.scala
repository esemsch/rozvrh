package x

import collection.mutable

object Rows extends App {
  def setBit(int:Int,index:Int) = {
    int | math.pow(2,index).toInt
  }
  val teachers = Data.data._2.toList
  val tiles = Data.data3.filter(j => true).map(j => {
    val clss = j.classHour.classes.foldLeft(0)((result,cls) => setBit(result,cls-1))
    val teacher = setBit(0,teachers.indexOf(j.teacher))
    Tile(clss,teacher,-1,j)
  }).zipWithIndex.map(ti => Tile(ti._1.classes,ti._1.teacher,ti._2,ti._1.job))
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
  val countsPerDay = tiles.map(t => Array(0,0,0,0,0)).toArray
  val placed = tiles.flatMap(t => (1 to t.job.count).map(x => Array(-1, -1, -1))).toArray
  val placedPerHour = (MONDAY to FRIDAY).map(d => (FIRST_HOUR to LAST_HOUR).map(h => 0).toArray).toArray
  val placedPerTeacher = {
    val retVal = teachers.map(t => 0).toArray
//    retVal(teachers.indexOf(Teacher("Iva"))) = -1000
//    retVal(teachers.indexOf(Teacher("Bohunka"))) = -100
//    retVal(teachers.indexOf(Teacher("Eva"))) = -10
//    retVal(teachers.indexOf(Teacher("Lucka"))) = -10
    retVal
  }
  val mapToPlaced  = tiles.foldLeft(new Array[Int](tiles.size))((arr,t) => {
    arr(t.id) = if (t.id==0) 0 else (arr(t.id-1)+counts(t.id-1))
    arr
  })
  List(Teacher("Iva") -> (Set(MONDAY,TUESDAY,WEDNESDAY,THURSDAY),0 to 7),
    Teacher("Iva") -> (Set(FRIDAY),(0 to 1)++(4 to 7)),
    Teacher("Bohunka") -> (Set(TUESDAY,WEDNESDAY,THURSDAY),0 to 7),
    Teacher("Eva") -> (Set(TUESDAY,FRIDAY),0 to 7),
    Teacher("Lucka") -> (Set(FRIDAY),0 to 7)).foreach(ta => {
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
      placedPerHour(d)(h) = placedPerHour(d)(h) + grades.size
    }))
  }
    freeHours(TUESDAY to FRIDAY,FIRST_GRADE+1 to FIRST_GRADE+4,List(0))
    freeHours(List(MONDAY),List(FIRST_GRADE+1),List(0))
    freeHours(MONDAY to FRIDAY,FIRST_GRADE+1 to FIRST_GRADE+2,6 to 7)
    freeHours(MONDAY to FRIDAY,FIRST_GRADE+3 to FIRST_GRADE+4,List(6))
    freeHours(TUESDAY to FRIDAY,FIRST_GRADE+3 to FIRST_GRADE+4,6 to 7)
    freeHours(List(TUESDAY,WEDNESDAY,FRIDAY),FIRST_GRADE+1 to LAST_GRADE,6 to 7)

    applyTile(tilesLookup("Lucka")(Set(5,6,7,8)),MONDAY,5)
    applyTile(tilesLookup("Lucka")(Set(5,6,7,8)),MONDAY,6)
    applyTile(tilesLookup("Lucka")(Set(5,6,7,8)),THURSDAY,5)
    applyTile(tilesLookup("Lucka")(Set(5,6,7,8)),THURSDAY,6)
//    applyTile(tilesLookup("Iva")(Set(6)),FRIDAY,2)
//    applyTile(tilesLookup("Iva")(Set(6)),FRIDAY,3)

  val teachersOrder = List("Iva","Bohunka","Eva","Lucka","Alena","Martina","Tereza","Gita","Hana")

  val tileRows = H.tileIndexRows.map(xr => xr.map(ri => tiles(ri)))
  val rows = tileRows.map(r => {
    val minInstances: Int = r.foldLeft(Integer.MAX_VALUE)((min, t) => {
      val instances = tileRows.filter(x => x != r && x.exists(t2 => t2 == t)).size
      if (instances < min) instances else min
    })
    (r,minInstances)
  }).toList.sortBy(x => -x._2).map(x => x._1.map(t => t.id).toArray).toArray
//  val rows = H.tileIndexRows.sortBy(r => {
//    r.map(ti => {
//      val tile = tiles(ti)
//      teachersOrder.indexOf(tile.job.teacher.name)
//    }).foldLeft(0)((sum,x) => sum - math.pow(2,10-x).toInt)
//  })

  def applicable(t:Tile,d:Int,h:Int) = {
    (((places(d)(h)(0) & t.classes) | (places(d)(h)(1) & t.teacher)) == 0)
  }

  def placedInd(t:Tile) = {
    mapToPlaced(t.id)+(counts(t.id)-1)
  }

  def applyTile(t:Tile,d:Int,h:Int) {
    places(d)(h)(0) = places(d)(h)(0) | t.classes
    places(d)(h)(1) = places(d)(h)(1) | t.teacher

    val pid = placedInd(t)
    placed(pid)(0) = d
    placed(pid)(1) = h
    placed(pid)(2) = t.id
    counts(t.id) = counts(t.id) - 1
    countsPerDay(t.id)(d) = countsPerDay(t.id)(d) + 1
    placedPerHour(d)(h) = placedPerHour(d)(h) + t.job.classHour.classes.size
    placedPerTeacher(teachers.indexOf(t.job.teacher)) = placedPerTeacher(teachers.indexOf(t.job.teacher)) + 1
  }

  def revertTile(t:Tile,d:Int,h:Int) {
    places(d)(h)(0) = places(d)(h)(0) ^ t.classes
    places(d)(h)(1) = places(d)(h)(1) ^ t.teacher

    placedPerTeacher(teachers.indexOf(t.job.teacher)) = placedPerTeacher(teachers.indexOf(t.job.teacher)) - 1
    placedPerHour(d)(h) = placedPerHour(d)(h) - t.job.classHour.classes.size
    countsPerDay(t.id)(d) = countsPerDay(t.id)(d)-1
    counts(t.id) = counts(t.id) + 1
    val pid = placedInd(t)
    placed(pid)(0) = -1
    placed(pid)(1) = -1
    placed(pid)(2) = -1
  }

  def rowApplicable(rowInd:Int,day:Int,hour:Int) = {
    rows(rowInd).forall(ti => counts(ti)>0 && applicable(tiles(ti),day,hour))
  }

  def rowEverApplicable(rowInd:Int) = {
    rows(rowInd).forall(ti => counts(ti)>0)
  }

  def applyRow(rowInd:Int,day:Int,hour:Int) {
    rows(rowInd).foreach(ti => {
      applyTile(tiles(ti),day,hour)
    })
  }

  def revertRow(rowInd:Int,day:Int,hour:Int) {
    rows(rowInd).foreach(ti => revertTile(tiles(ti),day,hour))
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
      open.filter(ri => rowApplicable(ri,day,hour)).toList.sortBy(ri => {
        rows(ri).foldLeft(0)((total,ti) => {
          total + placedPerTeacher(teachers.indexOf(tiles(ti).job.teacher))
        })
//        rows(ri).foldLeft(Integer.MAX_VALUE)((min,ti) => {
//          val ppt = placedPerTeacher(teachers.indexOf(tiles(ti).job.teacher))
//          if(min>ppt) ppt else min
//        })
      })
    }
    def isEmpty:Boolean = open.isEmpty
  }

  var cnt = 0
  val rowOpen = new RowOpen()
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
      println(placedPerTeacher.mkString(","))
    }
    if(day>FRIDAY) {
      true
    }
    else if(((day==TUESDAY || day==WEDNESDAY || day==FRIDAY) && hour==6)||((day != TUESDAY && day != WEDNESDAY && day!=FRIDAY) && hour==5)) {
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
}

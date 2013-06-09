package x

import collection.mutable

case class Tile(classes:Int,teacher:Int,id:Int,job:Job)

object Tiles extends App {
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
  val mapToPlaced  = tiles.foldLeft(new Array[Int](tiles.size))((arr,t) => {
    arr(t.id) = if (t.id==0) 0 else (arr(t.id-1)+counts(t.id-1))
    arr
  })
  Map(Teacher("Bohunka") -> Set(TUESDAY,WEDNESDAY,THURSDAY),Teacher("Eva") -> Set(TUESDAY,FRIDAY),Teacher("Lucka") -> Set(FRIDAY)).foreach(ta => {
    val index = teachers.indexOf(ta._1)
    ta._2.foreach(d => {
      (FIRST_HOUR to LAST_HOUR).foreach(h => places(d)(h)(1) = setBit(places(d)(h)(1),index))
    })
  })
  def freeHours(days:Seq[Int],grades:Seq[Int],hours:Seq[Int]) {
    days.foreach(d => hours.foreach(h => {
      grades.foreach(gr => {
        places(d)(h)(0) = setBit(places(d)(h)(0),(gr-1))
      })
      placedPerHour(d)(h) = placedPerHour(d)(h) + grades.size / 2
    }))
  }
  freeHours((MONDAY to FRIDAY),(FIRST_GRADE+1 to FIRST_GRADE+4),List(0))
  freeHours((MONDAY to FRIDAY),(FIRST_GRADE+1 to FIRST_GRADE+4),(6 to 7))
  freeHours(List(TUESDAY,WEDNESDAY,FRIDAY),(FIRST_GRADE+1 to LAST_GRADE),(6 to 7))

  applyTile(tilesLookup("Lucka")(Set(5,6,7,8)),MONDAY,5)
  applyTile(tilesLookup("Lucka")(Set(5,6,7,8)),MONDAY,6)
  applyTile(tilesLookup("Lucka")(Set(5,6,7,8)),THURSDAY,5)
  applyTile(tilesLookup("Lucka")(Set(5,6,7,8)),THURSDAY,6)
  applyTile(tilesLookup("Iva")(Set(6)),FRIDAY,2)
  applyTile(tilesLookup("Iva")(Set(6)),FRIDAY,3)

  def applicable(t:Tile,d:Int,h:Int) = {
    (((places(d)(h)(0) & t.classes) | (places(d)(h)(1) & t.teacher)) == 0) && (countsPerDay(t.id)(d)<=t.job.count/5)
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
    placedPerHour(d)(h) = placedPerHour(d)(h) + 1
  }

  def revertTile(t:Tile,d:Int,h:Int) {
    places(d)(h)(0) = places(d)(h)(0) ^ t.classes
    places(d)(h)(1) = places(d)(h)(1) ^ t.teacher

    placedPerHour(d)(h) = placedPerHour(d)(h) - 1
    countsPerDay(t.id)(d) = countsPerDay(t.id)(d)-1
    counts(t.id) = counts(t.id) + 1
    val pid = placedInd(t)
    placed(pid)(0) = -1
    placed(pid)(1) = -1
    placed(pid)(2) = -1
  }

  class Open {
    val hOrder = new HOrder(tiles,H.order2)

    var open = new mutable.HashSet[Tile]() {
      tiles.filter(t => counts(t.id)>0).foreach(t => add(t))
    }

    def popFromOpen(t:Tile) {
      if(counts(t.id)==0) {
        open.remove(t)
      }
    }

    def pushToOpen(t:Tile) = {
      if (counts(t.id)==1) {
        open.add(t)
      }
    }

    def options(day:Int,hour:Int) = {
      open.filter(t => applicable(t,day,hour)).toList.sortWith(hOrder.precedes(_,_))
    }

    def isEmpty = open.isEmpty

    override def toString = {
      open.map(o => o.job.toTeachersJob.toString + " - " + counts(o.id)).mkString("\n")
    }
  }

  def hourComplete(day:Int,hour:Int) = (places(day)(hour)(0) & 255) == 255

  var open = new Open()
  var cnt = 0
  val depthCounter = new Array[Int](1000)

  def search(day:Int,hour:Int,depth:Int,rowDepth:Int):Boolean = {
    cnt = cnt + 1
    depthCounter(depth) = depthCounter(depth) + 1
    if(cnt%100000==0) {
      println(cnt)
    }
    if(cnt%1000000==0) {
      Output.printTiles(places,tiles,placed)
      println(open)
    }
    if(open.isEmpty || day > FRIDAY) {
      true
    }
    else {
      if(hourComplete(day,hour)
        || (placedPerHour(day)(hour)>4 && depthCounter(depth)>100000)
      || (placedPerHour(day)(hour)>3 && depthCounter(depth)>500000)
      || (placedPerHour(day)(hour)>2 && depthCounter(depth)>2000000)
      ) {
        if (hour==7) {
          search(day+1,0,depth,0)
        } else {
          search(day,hour+1,depth,0)
        }
      } else {
        open.options(day,hour).exists(t => {
          applyTile(t,day,hour)
          open.popFromOpen(t)
          if(search(day,hour,(depth+1),(rowDepth+1))) {
            true
          } else {
            revertTile(t,day,hour)
            open.pushToOpen(t)
            false
          }
        })
      }
    }
  }

  var lineCount = 0
  var currLC = 0

  def searchForLines(depth:Int):Boolean = {
    if(hourComplete(0,0)) {
      lineCount = lineCount + 1
      if(lineCount%100000==0) {
        print(".")
      }
      false
    } else {
      open.options(0,0).exists(t => {
        if(depth==0) {
          currLC = lineCount
        }
        applyTile(t,0,0)
        open.popFromOpen(t)
        val ok = if(searchForLines((depth+1))) {
          true
        } else {
          revertTile(t,0,0)
          open.pushToOpen(t)
          false
        }
        if(depth==0) {
          print("\n"+t.job+" ----> "+(lineCount-currLC))
        }
        ok
      })
    }
  }

//  searchForLines(0)
  search(0,0,0,0)

  Output.printTiles(places,tiles,placed)

  println(open.open.map(t => t.job + " ---- "+counts(t.id)).mkString("\n"))

  println(Data.data2.foldLeft(0)((total,j) => total + j.count)-placed.filter(pl => (pl(2) != -1)).size)
}

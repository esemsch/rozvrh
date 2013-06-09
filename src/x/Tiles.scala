package x

import collection.mutable

case class Tile(classes:Int,teacher:Int,id:Int,job:Job)

object Tiles extends App {
  val teachers = Data.data._2.toList
  val tiles = Data.data2.filter(j => !j.classHour.pe).map(j => {
    def setBit(int:Int,index:Int) = {
      int | math.pow(2,index).toInt
    }
    val clss = j.classHour.classes.foldLeft(0)((result,cls) => setBit(result,cls-1))
    val teacher = setBit(0,teachers.indexOf(j.teacher))
    Tile(clss,teacher,-1,j)
  }).zipWithIndex.map(ti => Tile(ti._1.classes,ti._1.teacher,ti._2,ti._1.job))
  val places = (MONDAY to FRIDAY).map(d => (FIRST_HOUR to LAST_HOUR).map(h => Array(0,0)).toArray).toArray
  val counts = tiles.map(t => t.job.count).toArray
  val placed = tiles.flatMap(t => (1 to t.job.count).map(x => Array(-1, -1, -1))).toArray
  val mapToPlaced  = tiles.foldLeft(new Array[Int](tiles.size))((arr,t) => {
    arr(t.id) = if (t.id==0) 0 else (arr(t.id-1)+counts(t.id-1))
    arr
  })
  println("")

  def applicable(t:Tile,d:Int,h:Int) = {
    ((places(d)(h)(0) & t.classes) | (places(d)(h)(1) & t.teacher)) == 0
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
  }

  def revertTile(t:Tile,d:Int,h:Int) {
    places(d)(h)(0) = places(d)(h)(0) ^ t.classes
    places(d)(h)(1) = places(d)(h)(1) ^ t.teacher

    counts(t.id) = counts(t.id) + 1
    val pid = placedInd(t)
    placed(pid)(0) = -1
    placed(pid)(1) = -1
    placed(pid)(2) = -1
  }

  class Open {
    val hOrder = new HOrder(tiles,H.order)

    var open = new mutable.HashSet[Tile]() {
      tiles.foreach(t => add(t))
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
    if(cnt%10000==0) {
      println(depth+" "+rowDepth+" "+depthCounter(depth))
    }
    if(cnt%100000==0) {
      println(cnt)
    }
    if(cnt%1000000==0) {
      Output.printTiles(places,tiles,placed)
      println(open)
    }
    if(open.isEmpty || day == FRIDAY && hour == 5) {
      true
    }
    else {
      if(hourComplete(day,hour) ||
        (rowDepth>4 && depthCounter(depth)>10000) ||
        (rowDepth>3 && depthCounter(depth)>50000) ||
        (rowDepth>2 && depthCounter(depth)>200000)) {
        if (hour==5) {
          search(day+1,1,depth,0)
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
//          println("Count started for: "+t.job)
          currLC = lineCount
        }
//        if(depth == 1) {
//          println(t.job+" D = "+depth)
//        }
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
//          open.popFromOpen(t)
        }
        ok
      })
    }
  }

  searchForLines(0)
//  search(0,0,0,0)

  Output.printTiles(places,tiles,placed)

}

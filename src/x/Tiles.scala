package x

import collection.mutable

case class Tile(classes:Int,teacher:Int,id:Int,job:Job)

object Tiles extends App {
  val teachers = Data.data._2.toList
  val tiles = Data.data2.filter(j => !j.classHour.pe).flatMap(j => {
    def setBit(int:Int,index:Int) = {
      int | math.pow(2,index).toInt
    }
    val clss = j.classHour.classes.foldLeft(0)((result,cls) => setBit(result,cls-1))
    val teacher = setBit(0,teachers.indexOf(j.teacher))
    List[Tile]().padTo(j.count,Tile(clss,teacher,-1,j))
  }).zipWithIndex.map(ti => Tile(ti._1.classes,ti._1.teacher,ti._2,ti._1.job))
  val places = (MONDAY to FRIDAY).map(d => (FIRST_HOUR to LAST_HOUR).map(h => Array(0,0)).toArray).toArray
  val placed = tiles.map(t => Array(-1,-1))

  def applicable(t:Tile,d:Int,h:Int) = {
    ((places(d)(h)(0) & t.classes) | (places(d)(h)(1) & t.teacher)) == 0
  }

  def applyTile(t:Tile,d:Int,h:Int) {
    places(d)(h)(0) = places(d)(h)(0) | t.classes
    places(d)(h)(1) = places(d)(h)(1) | t.teacher
    placed(t.id)(0) = d
    placed(t.id)(1) = h
  }

  def revertTile(t:Tile,d:Int,h:Int) {
    places(d)(h)(0) = places(d)(h)(0) ^ t.classes
    places(d)(h)(1) = places(d)(h)(1) ^ t.teacher
    placed(t.id)(0) = -1
    placed(t.id)(1) = -1
  }

  def hourComplete(day:Int,hour:Int) = (places(day)(hour)(0) & 255) == 255

  var cnt = 0

  class Open {
    var open = new mutable.HashSet[Tile]() {
      tiles.foreach(t => add(t))
    }

    def popFromOpen(t:Tile) = {
      open.remove(t)
    }

    def pushToOpen(t:Tile) = {
      open.add(t)
    }

    def options(day:Int,hour:Int):mutable.HashSet[Tile] = {
      open.filter(t => applicable(t,day,hour))
    }

    def isEmpty = open.isEmpty

    override def toString = {
      open.map(o => o.job.toTeachersJob.toString).mkString("\n")
    }
  }

  var open = new Open()

  def search(day:Int,hour:Int):Boolean = {
    cnt = cnt + 1
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
      if(hourComplete(day,hour)) {
        if (hour==5) {
          search(day+1,1)
        } else {
          search(day,hour+1)
        }
      } else {
        open.options(day,hour).exists(t => {
          applyTile(t,day,hour)
          open.popFromOpen(t)
          if(search(day,hour)) {
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

  search(0,1)

  Output.printTiles(places,tiles,placed)

}

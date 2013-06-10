package x

import collection.mutable

class TilesSolver(tiles:List[Tile],places:Array[Array[Array[Int]]],counts:Array[Int],placed:Array[Array[Int]],mapToPlaced:Array[Int]) {
  val placedPerHour = (MONDAY to FRIDAY).map(d => (FIRST_HOUR to LAST_HOUR).map(h => 0).toArray).toArray

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
    placedPerHour(d)(h) = placedPerHour(d)(h) + t.job.classHour.classes.size
  }

  def revertTile(t:Tile,d:Int,h:Int) {
    places(d)(h)(0) = places(d)(h)(0) ^ t.classes
    places(d)(h)(1) = places(d)(h)(1) ^ t.teacher

    placedPerHour(d)(h) = placedPerHour(d)(h) - t.job.classHour.classes.size
    counts(t.id) = counts(t.id) + 1
    val pid = placedInd(t)
    placed(pid)(0) = -1
    placed(pid)(1) = -1
    placed(pid)(2) = -1
  }

  def hourComplete(day:Int,hour:Int,grades:Int = 255) = {
    (places(day)(hour)(0) & grades) == grades
  }

  class Open(grades:Set[Int] = Set(1,2,3,4,5,6,7,8)) {
    val hOrder = new HOrder(tiles,H.order2)

    var open = new mutable.HashSet[Tile]() {
      tiles.filter(t => t.job.classHour.classes.exists(cls => grades.contains(cls))).filter(t => counts(t.id)>0).foreach(t => add(t))
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
      open.filter(t => applicable(t,day,hour)).toList.sortBy(t => t.job.classHour.lowestClass)
    }

    def isEmpty = open.isEmpty

    override def toString = {
      open.map(o => o.job.toTeachersJob.toString + " - " + counts(o.id)).mkString("\n")
    }
  }

  def solve = {
    val daysOrder = Array(FRIDAY,MONDAY,TUESDAY,WEDNESDAY,THURSDAY,1000)

    val open = new Open()
    var cnt = 0
    val depthCounter = new Array[Int](1000)

    def search(dayNumber:Int,hour:Int,depth:Int,rowDepth:Int):Boolean = {
      val day = daysOrder(dayNumber)
      cnt = cnt + 1
      depthCounter(depth) = depthCounter(depth) + 1
      if(cnt%1000000==0) {
        println(cnt)
      }
      if(cnt%10000000==0) {
        Output.printTiles(places,tiles,placed)
        println(open)
      }
      if(open.isEmpty) {
        true
      }
      else if(day>FRIDAY) {
        search(0,0,0,0)
      }
      else {
        if(hourComplete(day,hour) || rowDepth>0)
        {
          if (hour==7) {
            search(dayNumber+1,0,depth,0)
          } else {
            search(dayNumber,hour+1,depth,0)
          }
        } else {
          open.options(day,hour).exists(t => {
            applyTile(t,day,hour)
            open.popFromOpen(t)
            if(search(dayNumber,hour,(depth+1),(rowDepth+1))) {
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

    search(0,0,0,0)

    Output.printTiles(places,tiles,placed)

    //  println(open.open.map(t => t.job + " ---- "+counts(t.id)).mkString("\n"))

    println(Data.data2.foldLeft(0)((total,j) => total + j.count)-placed.filter(pl => (pl(2) != -1)).size)
  }

  def calcRows(day:Int,hour:Int,completeForGrades:Set[Int]) = {

    val open = new Open(completeForGrades)

    var lineCount = 0
    var currLC = 0

    val lines:mutable.SetBuilder[Set[Int],Set[Set[Int]]] = new mutable.SetBuilder[Set[Int],Set[Set[Int]]](Set[Set[Int]]())

    val grades = completeForGrades.foldLeft(0)((out,gr) => setBit(out,(gr-1)))

    def searchForLines(depth:Int,line:Set[Int]):Boolean = {
      if(hourComplete(day,hour,grades)) {
        lineCount = lineCount + 1
        if(lineCount%100000==0) {
          print(".")
        }
        lines += line
        false
      } else {
        open.options(day,hour).exists(t => {
          if(depth==0) {
            currLC = lineCount
          }
          applyTile(t,day,hour)
          open.popFromOpen(t)
          val ok = if(searchForLines((depth+1),(line + t.id))) {
            true
          } else {
            revertTile(t,day,hour)
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

    searchForLines(0,Set[Int]())

    lines.result()
  }
}

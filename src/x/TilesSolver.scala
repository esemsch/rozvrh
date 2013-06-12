package x

import collection.mutable

class TilesSolver(tiles:Array[Tile],places:Array[Array[Array[Int]]],counts:Array[Int],placed:Array[Array[Int]],mapToPlaced:Array[Int],tilesPerDay:Array[(Array[Int],Tile)]) {
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
    tilesPerDay(t.id)._1(d) = tilesPerDay(t.id)._1(d) + 1
  }

  def revertTile(t:Tile,d:Int,h:Int) {
    places(d)(h)(0) = places(d)(h)(0) ^ t.classes
    places(d)(h)(1) = places(d)(h)(1) ^ t.teacher

    tilesPerDay(t.id)._1(d) = tilesPerDay(t.id)._1(d) - 1
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
      open.filter(t => applicable(t,day,hour)).toList.sortBy(t => -t.job.classHour.lowestClass)
    }

    def isEmpty = open.isEmpty

    override def toString = {
      open.map(o => o.job.toTeachersJob.toString + " - " + counts(o.id)).mkString("\n")
    }
  }

  def solve = {
    val daysOrder = Array(FRIDAY,MONDAY,TUESDAY,WEDNESDAY,THURSDAY,1000)
//    val daysOrder = Array(MONDAY,TUESDAY,WEDNESDAY,THURSDAY,FRIDAY,1000)

    val open = new Open()
    var cnt = 0
    val depthCounter = new Array[Int](1000)

    var best = 5

    def search(dayNumber:Int,hour:Int,depth:Int,rowDepth:Int,doTry:Boolean,maxDepth:Int):Boolean = {
      val day = daysOrder(dayNumber)
      cnt = cnt + 1
      depthCounter(depth) = depthCounter(depth) + 1
      if(cnt%1000000==0) {
        println(cnt)
        println(depthCounter.filter(x => x>0).zipWithIndex.map(x=>x._2+" = "+x._1).mkString("\n"))
      }
//      if(cnt%10000000==0) {
//        Output.printTiles(places,tiles,placed)
//        println(open)
//      }
      val openSize = open.open.foldLeft(0)((tot,t) => tot + counts(t.id))
      if(openSize<best) {
        Output.printTiles(places,tiles,placed)
        println(open)
        best = openSize
      }
      if(open.isEmpty) {
        true
      }
      else if(depth==maxDepth || day>FRIDAY) {
        false
//        search(0,0,0,0,true)
      }
      else {
        if(hourComplete(day,hour) || !doTry)
        {
          if (hour==7) {
            search(dayNumber+1,0,depth,0,true,maxDepth)
          } else {
            search(dayNumber,hour+1,depth,0,true,maxDepth)
          }
        } else {
          def searchFunc(doTryNow:Boolean) = {
            val options: List[Tile] = open.options(day, hour).take(2)
            if(options.isEmpty && rowDepth==0) {
              search(dayNumber,hour,(depth+1),(rowDepth+1),doTryNow,maxDepth)
//              Output.printTiles(places,tiles,placed)
//              println("Options empty! Row depth = "+rowDepth+" open size = "+open.open.size+" day = "+day+" hour = "+hour)
            } else {
              options.exists(t => {
                applyTile(t,day,hour)
                open.popFromOpen(t)
                if(search(dayNumber,hour,(depth+1),(rowDepth+1),doTryNow,maxDepth)) {
                  true
                } else {
                  revertTile(t,day,hour)
                  open.pushToOpen(t)
                  false
                }
              })
            }
          }
          if(rowDepth==0) {
            searchFunc(true)
          } else {
            if(!searchFunc(true)) {
              val found = searchFunc(false)
              if(!found) {
                search(dayNumber,hour,(depth+1),(rowDepth+1),false,maxDepth)
              } else false
            } else true
          }
        }
      }
    }

    def search2:Boolean = {
      val openSize = open.open.foldLeft(0)((tot,t) => tot + counts(t.id))
      if(openSize<best) {
        Output.printTiles(places,tiles,placed)
        println(open)
        best = openSize
      }
      cnt = cnt + 1
      if(cnt%1000000==0) {
        println(cnt)
      }
      if (open.isEmpty) {
        true
      } else {
        val jobsByTeacher = open.open.map(t => t.job.teacher).foldLeft(Map[Teacher,Int]())((tcnts,teacher) => {
          tcnts.get(teacher) match {
            case None => tcnts + (teacher -> 1)
            case Some(tcnt) => tcnts + (teacher -> (1+tcnt))
          }
        })
        val options = (MONDAY to FRIDAY).flatMap(d => (FIRST_HOUR to LAST_HOUR).map(h => (d,h))).flatMap(dh => {
          open.options(dh._1,dh._2).map(t => (t,dh))
        }).sortBy(x => -jobsByTeacher(x._1.job.teacher))
        options.exists(opt => {
          val t = opt._1
          val day = opt._2._1
          val hour = opt._2._2
          applyTile(t,day,hour)
          open.popFromOpen(t)
          if (search2) true
          else {
            revertTile(t,day,hour)
            open.pushToOpen(t)
            false
          }
        })
      }
    }

//    def search3:Boolean = {
//      val openSize = open.open.foldLeft(0)((tot,t) => tot + counts(t.id))
//      if(openSize<best) {
//        Output.printTiles(places,tiles,placed)
//        println(open)
//        best = openSize
//      }
//      cnt = cnt + 1
//      if(cnt%1000==0) {
//        println(cnt)
//      }
//      if (open.isEmpty) {
//        true
//      } else {
//        val opts = (MONDAY to FRIDAY).flatMap(d => (FIRST_HOUR to LAST_HOUR).map(h => (d,h))).map(dh => (findMaxRow(dh._1,dh._2),dh))
//          .filter(x => !x._1.isEmpty)
//        options.exists(opt => {
//          val ts = opt._1
//          val day = opt._2._1
//          val hour = opt._2._2
//          ts.foreach(t => {
//            applyTile(t,day,hour)
//            open.popFromOpen(t)
//          })
//          if (search3) true
//          else {
//            ts.foreach(t => {
//              revertTile(t,day,hour)
//              open.pushToOpen(t)
//            })
//            false
//          }
//        })
//      }
//    }

    search(0,0,0,0,true,100)
//    (open.open.foldLeft(0)((tot,t) => tot + counts(t.id)) to 100).exists(maxDepth => search(0,0,0,0,true,maxDepth))
//    search3
  }

  class RowCalculator(day:Int,hour:Int,completeForGrades:Set[Int] = Set(1,2,3,4,5,6,7,8)) {

    val open = new Open(completeForGrades)
    var lineCount = 0
    var currLC = 0
    val lines:mutable.SetBuilder[Set[Int],Set[Set[Int]]] = new mutable.SetBuilder[Set[Int],Set[Set[Int]]](Set[Set[Int]]())
    val grades = completeForGrades.foldLeft(0)((out,gr) => setBit(out,(gr-1)))
    var maxLine = Set[Set[Int]]()
    var maxSize = 1

    def searchForLines(depth:Int,line:Set[Int]):Boolean = {
      if(line.size>=maxSize) {
//        println(line)
        if(line.size>maxSize) {
          maxSize = line.size
          maxLine = Set[Set[Int]]()
        }
        maxLine = maxLine + line
      }
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
//            print("\n"+t.job+" ----> "+(lineCount-currLC))
          }
          ok
        })
      }
    }
  }

  def calcRows(day:Int,hour:Int,completeForGrades:Set[Int]) = {
    val rc = new RowCalculator(day,hour,completeForGrades)

    rc.searchForLines(0,Set[Int]())

    rc.lines.result()
  }

  def findMaxRow(day:Int,hour:Int) = {
    val rc = new RowCalculator(day,hour)

    rc.searchForLines(0,Set[Int]())

    rc.maxLine.map(x => x.map(y => tiles(y)))
  }
}

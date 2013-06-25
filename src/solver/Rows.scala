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
    Teacher("Gita") -> (Set(THURSDAY),3 to 7)).foreach(ta => {
    val index = teachers.indexOf(ta._1)
    ta._2._1.foreach(d => {
      ta._2._2.foreach(h => places(d)(h)(1) = setBit(places(d)(h)(1),index))
    })
  })

    val daysOrder = Array(THURSDAY,TUESDAY,WEDNESDAY,MONDAY,FRIDAY,1000)
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

  tilesSolver.applyTile(tilesLookup("Hana")(Set(5,6,7,8)),MONDAY,5)
  tilesSolver.applyTile(tilesLookup("Lucka")(Set(5,6,7,8)),MONDAY,6)
  tilesSolver.applyTile(tilesLookup("Lucka")(Set(5,6,7,8)),THURSDAY,5)
  tilesSolver.applyTile(tilesLookup("Hana")(Set(5,6,7,8)),THURSDAY,6)

  tilesSolver.applyTile(tilesLookup("Tereza")(Set(7,8)),FRIDAY,0)
  tilesSolver.applyTile(tilesLookup("Tereza")(Set(7,8)),WEDNESDAY,0)
  tilesSolver.applyTile(tilesLookup("Tereza")(Set(3,4)),TUESDAY,5)
  tilesSolver.applyTile(tilesLookup("Tereza")(Set(5,6)),THURSDAY,0)
  tilesSolver.applyTile(tilesLookup("Hana")(Set(7,8)),TUESDAY,0)
  tilesSolver.applyTile(tilesLookup("Hana")(Set(7,8)),THURSDAY,0)
  tilesSolver.applyTile(tilesLookup("Bohunka")(Set(8)),FRIDAY,5)

  val teachersOrder = List("Iva","Bohunka","Eva")

  val rows = H.tileIndexRows.sortBy(r => {
    r.map(ti => {
      val tile = tiles(ti)
      teachersOrder.indexOf(tile.job.teacher.name)
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

  def filterRows(day:Int,hour:Int,subjs:List[(String,Set[String],Set[Int])]):(Int,Int,List[Int]) = {
    val options = rowOpen.options(day, hour)
    val ret = options.filter(ri => {
      subjs.forall(x => {
        rows(ri).exists(ti => {
          val t = tiles(ti)
          (x._1 == null || t.job.teacher.name == x._1) &&
          (x._2 == null || (t.job.classHour.subjects diff x._2).size < t.job.classHour.subjects.size) &&
          (x._3 == null || (x._3.toSet == t.job.classHour.classes))
        })
      })
    })

    ret.foreach(ri => {
      val r = rows(ri)
      println(r.map(ti => tiles(ti).job).mkString(","))
    })

    println("===============================================")
    (day,hour,ret)
  }
  def filterRows(day:Int,hour:Int,teacher:String):(Int,Int,List[Int]) = {
    filterRows(day,hour,List[(String,Set[String],Set[Int])]((teacher,null,null)))
  }
  def filterRows(day:Int,hour:Int,teacher:String,subject:String,grade:Int):(Int,Int,List[Int]) = {
    filterRows(day,hour,List[(String,Set[String],Set[Int])]((teacher,Set[String](subject),Set[Int](grade))))
  }
  def filterRows(day:Int,hour:Int,teacher:String,subject:String,grades:Set[Int]):(Int,Int,List[Int]) = {
    filterRows(day,hour,List[(String,Set[String],Set[Int])]((teacher,if(subject!=null)Set[String](subject) else null,grades)))
  }
  def preassignRow(candidateRows:(Int,Int,List[Int])) {
    val ri = candidateRows._3.head
    applyRow(ri,candidateRows._1,candidateRows._2)
    rowOpen.popFromOpen(ri)
  }

  preassignRow(filterRows(TUESDAY,4,"Tereza",null,Set(3,4)))
  preassignRow(filterRows(THURSDAY,1,"Tereza",null,Set(5,6)))
  preassignRow(filterRows(TUESDAY,2,"Iva",null,Set(6)))
  preassignRow(filterRows(TUESDAY,3,"Iva",null,Set(6)))
  preassignRow(filterRows(WEDNESDAY,2,"Iva",null,Set(7)))
  preassignRow(filterRows(WEDNESDAY,3,"Iva",null,Set(7)))

//  preassignRow(filterRows(MONDAY,1,"Tereza",null,Set(5,6)))
//  preassignRow(filterRows(TUESDAY,2,"Martina","Vv",null))
//  preassignRow(filterRows(TUESDAY,3,"Martina","Vv",Set(4)))
//  preassignRow(filterRows(MONDAY,3,List(("Alena",null,null),(null,Set("Vv"),Set(3)),(null,Set("D"),Set(7)))))
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
      rowTiles.foldLeft(0)((tot,t) => {
        val teacherInd = teachersOrder.indexOf(t.job.teacher.name)
        val to = if(teacherInd>=0) (teacherInd-teachersOrder.size) else 0
        tot + to
      })
    }
    def spread = {
      val tilesPerThisDay = tilesPerDay.filter(td => td._1(day)>0)
      rowTiles.foldLeft(0)((tot,t) => {
        tot + (if(tilesPerThisDay.exists(_._2==t)) 1 else 0)
      })
    }
    def combined = {
      if(hour>=1 && hour <=3) {
        rowTiles.foldLeft(0)((tot,t) => tot + (if(t.job.classHour.combinedClasses) -1 else 0))
      } else {
        rowTiles.foldLeft(0)((tot,t) => tot + (if(t.job.classHour.combinedClasses) 1 else 0))
      }
    }
    def main = {
      if(hour>=1 && hour <=4) {
        rowTiles.foldLeft(0)((tot,t) => tot + (if(t.job.classHour.mainSubject) -1 else 0))
      } else {
        rowTiles.foldLeft(0)((tot,t) => tot + (if(t.job.classHour.mainSubject) 1 else 0))
      }
    }
    //    println(rowTiles.map(t=>t.job).mkString(",")+" --- Teachers = "+teachers+" Spread = "+spread+" Combined = "+combined+" Main = "+main)
    teachers + spread + combined + main
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
      search(dayIndex,(hour+1))
    }
//    else if(((day==TUESDAY || day==WEDNESDAY) && hour>=6)||((day != TUESDAY && day != WEDNESDAY) && hour>=5)) {
//    else if(((day==TUESDAY || day==WEDNESDAY || day==FRIDAY) && hour>=6)||((day != TUESDAY && day != WEDNESDAY && day!=FRIDAY) && hour>=5)) {
//    else if(((day==TUESDAY) && hour>=6)||((day != TUESDAY) && hour>=5)) {
    else if(hour>=5) {
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

  val schoolSchedule = Straightener.straighten2(Conversions.tilesToJobsArray(places,tiles,placed),jobs._2.toMap)

  Output.printSchedule(schoolSchedule)

  Checker.check(schoolSchedule,jobs._2.flatMap(x => x._2).toList)
}

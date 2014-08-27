package solver

import collection.mutable

class RowOpen(solverStateP:SolverState,rowsP:Array[Array[Int]]) {

  val rows = rowsP
  val solverState = solverStateP
  val tiles = solverStateP.tiles
  val tilesSolver = solverStateP.tilesSolver
  val counts = solverStateP.counts

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

  def calcConstraints(day:Int,hour:Int,rowInd:Int) = {
    val row = rows(rowInd)
    val rowTiles = row.map(ti => tiles(ti))

    def teachersSpread = {
      val tilesPerThisDay = solverState.tilesPerDay.filter(td => td._1(day)>0).map(td => (td._2,td._1(day)))
      rowTiles.foldLeft(0.0)((sum,t) => {
        val sumT = tilesPerThisDay.filter(x => x._1.teacher == t.teacher).foldLeft(0)((a,b) => a + b._2)
        sum + sumT
      })/rowTiles.size
    }
    def spread = {
      val tilesPerThisDay = solverState.tilesPerDay.filter(td => td._1(day)>0)
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
    spread + combined + main + teachersSpread
  }
}

object Rows {

  def rowOpenFactory(solverState:SolverState) = {
    val rows = H.tileIndexRows
    new RowOpen(solverState,rows)
  }

  def factory (solverState:SolverState,daysOrder: Array[Int]) = {
    new RowsSolver(rowOpenFactory(solverState),daysOrder)
  }


}

class RowsSolver(rowOpen:RowOpen,daysOrder: Array[Int]) {

  val rows = rowOpen.rows
  val solverState = rowOpen.solverState
  val tilesSolver = rowOpen.solverState.tilesSolver
  val counts = rowOpen.solverState.counts
  val tiles = rowOpen.solverState.tiles

  var cnt = 0
  def search(dayIndex:Int,hour:Int):Boolean = {
    val day = daysOrder(dayIndex)
    cnt = cnt + 1
    if(cnt%10000==0) {
      println(cnt)
    }
    if(cnt%100000==0) {
      Output.printTiles(solverState)
      println(Data.data2.foldLeft(0)((total,j) => total + j.count)-solverState.placed.filter(pl => (pl(2) != -1)).size)
      tiles
        .filter(t => counts(t.id)>0)
        .sortWith((t1,t2) => t1.job.teacher.name < t2.job.teacher.name)
        .foreach(t => println(t.job+" --- "+counts(t.id)))
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
        rowOpen.applyRow(rowInd,day,hour)
        rowOpen.popFromOpen(rowInd)
        if(!search(dayIndex+1,if(day==daysOrder(4)) (hour+1) else hour)) {
          rowOpen.revertRow(rowInd,day,hour)
          rowOpen.pushToOpen(rowInd)
          false
        } else true
      })
    }
  }


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

}

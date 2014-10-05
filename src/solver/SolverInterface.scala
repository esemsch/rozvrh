package solver

object SolverInterface {

  private val solverState = TilesSolver.factory

  HardConstraints.applyHardConstraints(solverState)
  private val rowOpen = Rows.rowOpenFactory(solverState)

  def getSolverState = solverState

  def getRowOpen = rowOpen

  def filterRows(day:Int,hour:Int,subjs:List[(String,Set[Int])]):(Int,Int,List[Int]) = {
    val options = rowOpen.options(day, hour)
    val ret = options.filter(ri => {
      subjs.forall(x => {
        rowOpen.rows(ri).exists(ti => {
          val t = solverState.tiles(ti)
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
    println(Output.printDayAndHour(candidateRows._1,candidateRows._2))
    candidateRows._3.zipWithIndex.foreach(r => println(r._2+": "+Output.printRow(rowOpen.rows(r._1).map(ti => solverState.tiles(ti)))))
    val options: List[Array[String]] = candidateRows._3.map(r => {
      val linearized: Array[TeachersJob] = rowOpen.rows(r).flatMap(ti => solverState.tiles(ti).job.toTeachersJob.linearize)
      linearized
        .sortWith((tj1, tj2) => tj1.classHour.lowestClass < tj2.classHour.lowestClass)
        .map(tj => tj.classHour.subject.split(" ")(0) + " ("+tj.teacher.name+")")
        .toArray
    })
    if(options.isEmpty) {
      System.exit(1)
    }
    ScheduleVisualisation.vis.highlight(candidateRows._1,candidateRows._2,java.awt.Color.RED)
    val ind = new RowDialog(Output.printDayAndHour(candidateRows._1,candidateRows._2),options).selected.getOrElse(0)
    val ri = candidateRows._3(ind)
    ScheduleVisualisation.vis.highlight(candidateRows._1,candidateRows._2,java.awt.Color.LIGHT_GRAY)
    rowOpen.applyRow(ri,candidateRows._1,candidateRows._2)
    rowOpen.popFromOpen(ri)
    ScheduleVisualisation.vis.refresh(Transformations.solverStateToSchoolSchedule(solverState))
  }

  def applyTile(teachersName:String, classesAndMain:(Set[Int], Boolean), day:Int, `class`:Int) {
    val tilesSolver = solverState.tilesSolver
    val tilesLookup = solverState.tilesLookup

    tilesSolver.applyTile(tilesLookup(teachersName)(classesAndMain), day, `class`)
  }

}

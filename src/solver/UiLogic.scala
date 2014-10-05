package solver

class UiLogic(solver:SolverInterface) {

  def displayRowDialog(candidateRows:(Int,Int,List[Int])) = {
    val rowOpen = solver.getRowOpen
    val solverState = solver.getSolverState

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
    highlight(candidateRows._1,candidateRows._2,java.awt.Color.RED)
    val ind = new RowDialog(Output.printDayAndHour(candidateRows._1,candidateRows._2),options).selected.getOrElse(0)

    highlight(candidateRows._1,candidateRows._2,java.awt.Color.LIGHT_GRAY)

    candidateRows._3(ind)
  }

  def highlight(day:Int, hour:Int, color:java.awt.Color) : Unit = {
    ScheduleVisualisation.vis.highlight(day,hour,java.awt.Color.LIGHT_GRAY)
  }

  def refresh: Unit = {
    ScheduleVisualisation.vis.refresh(Transformations.solverStateToSchoolSchedule(solver.getSolverState))
  }

  def displayRowCandidatesDialog = {

  }


}

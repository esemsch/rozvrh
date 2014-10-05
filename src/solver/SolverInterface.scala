package solver

class SolverInterface {

  private val solverState = HardConstraints.applyHardConstraints(TilesSolver.factory)
  private val rowOpen = Rows.rowOpenFactory(solverState)
  private var rowsSolver:RowsSolver = null

  def getSolverState = solverState
  def getRowOpen = rowOpen

  def getCandidateRows(day:Int,hour:Int,subjs:List[(String,Set[Int])]):(Int,Int,List[Int]) = {
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
  def getCandidateRows(day:Int,hour:Int,teacher:String):(Int,Int,List[Int]) = {
    getCandidateRows(day,hour,List[(String,Set[Int])]((teacher,null)))
  }
  def getCandidateRows(day:Int,hour:Int,teacher:String,grades:Set[Int]):(Int,Int,List[Int]) = {
    getCandidateRows(day,hour,List[(String,Set[Int])]((teacher,grades)))
  }

  def preassignRow(rowIndex:Int,day:Int,hour:Int) {
    rowOpen.applyRow(rowIndex,day,hour)
    rowOpen.popFromOpen(rowIndex)
    ScheduleVisualisation.vis.refresh(Transformations.solverStateToSchoolSchedule(solverState))
  }

  def applyTile(teachersName:String, classesAndMain:(Set[Int], Boolean), day:Int, `class`:Int) {
    val tilesSolver = solverState.tilesSolver
    val tilesLookup = solverState.tilesLookup

    tilesSolver.applyTile(tilesLookup(teachersName)(classesAndMain), day, `class`)
  }

  def runRowSolver: Unit = {
    // HEURISTICS
    val daysOrder = Array(
      THURSDAY,TUESDAY,WEDNESDAY,MONDAY,FRIDAY,
      THURSDAY,TUESDAY,WEDNESDAY,MONDAY,FRIDAY,
      THURSDAY,TUESDAY,WEDNESDAY,MONDAY,FRIDAY,
      THURSDAY,TUESDAY,WEDNESDAY,MONDAY,FRIDAY,
      1000)

    rowsSolver = Rows.factory(solverState,daysOrder)

    rowsSolver.search(0,1)
  }

  def runTilesSolver: Unit = {
    rowsSolver.maxRowSearch(0,1)
  }

}

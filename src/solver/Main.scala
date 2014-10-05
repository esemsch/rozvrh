package solver

object Main extends App {

  val solverState = SolverInterface.getSolverState

  // PRE-PLACEMENTS
  SolverInterface.applyTile("Hana",    (Set(5,6,7,8),  false), MONDAY,    5)
  SolverInterface.applyTile("Hana",    (Set(5,6,7,8),  false), MONDAY,    6)
  SolverInterface.applyTile("Hana",    (Set(5,6,7,8),  false), THURSDAY,  5)
  SolverInterface.applyTile("Hana",    (Set(5,6,7,8),  false), THURSDAY,  6)

  SolverInterface.applyTile("Martina", (Set(2,4),      true),  MONDAY,    5)
  SolverInterface.applyTile("Martina", (Set(3),        true),  WEDNESDAY, 0)
  SolverInterface.applyTile("Martina", (Set(2,4),      true),  FRIDAY,    5)
  SolverInterface.applyTile("Martina", (Set(3),        true),  WEDNESDAY, 5)
  SolverInterface.applyTile("Martina", (Set(1),        true),  TUESDAY,   5)
  SolverInterface.applyTile("Martina", (Set(1),        true),  THURSDAY,  5)

  SolverInterface.applyTile("Tereza",  (Set(7,8),      false), TUESDAY,   7)

  SolverInterface.applyTile("Lucka",   (Set(7,8),      false), TUESDAY,   6)
  SolverInterface.applyTile("Lucka",   (Set(2,4),      false), WEDNESDAY, 5)

  ScheduleVisualisation.vis.refresh(Transformations.solverStateToSchoolSchedule(solverState))

  // PRE-ASSIGNMENTS
  SolverInterface.preassignRow(SolverInterface.filterRows(TUESDAY,2,List(
    ("Iva",Set(6,7)),
    ("Lucka",Set(8))
  )))
  SolverInterface.preassignRow(SolverInterface.filterRows(TUESDAY,3,List(
    ("Iva",Set(8)),
    ("Lucka",Set(7))
  )))

  SolverInterface.preassignRow(SolverInterface.filterRows(WEDNESDAY,2,"Iva",Set(6,7)))
  SolverInterface.preassignRow(SolverInterface.filterRows(WEDNESDAY,3,List(
    ("Iva",Set(8)),
    ("Lucka",Set(7))
  )))

  SolverInterface.preassignRow(SolverInterface.filterRows(FRIDAY,4,List(
    ("Hana",Set(2,3,4)),
    ("Bohunka",Set(6,7,8))
  )))

  Output.saveSolverState("solver-state.json",solverState)

  Stats.printStats(solverState.tiles,SolverInterface.getRowOpen.rows,solverState.jobs._1);

  // HEURISTICS
  val daysOrder = Array(
    THURSDAY,TUESDAY,WEDNESDAY,MONDAY,FRIDAY,
    THURSDAY,TUESDAY,WEDNESDAY,MONDAY,FRIDAY,
    THURSDAY,TUESDAY,WEDNESDAY,MONDAY,FRIDAY,
    THURSDAY,TUESDAY,WEDNESDAY,MONDAY,FRIDAY,
    1000)

  val rowSolver = Rows.factory(solverState,daysOrder)

  rowSolver.search(0,1)

  Output.printTiles(solverState)
  ScheduleVisualisation.vis.refresh(Transformations.solverStateToSchoolSchedule(solverState))

  Stats.printTilesToGo(solverState)

  rowSolver.maxRowSearch(0,1)

  Output.printSchedule(Transformations.solverStateToSchoolSchedule(solverState))
  ScheduleVisualisation.vis.refresh(Transformations.solverStateToSchoolSchedule(solverState))

  val schoolSchedule = Straightener.straighten2(Transformations.solverStateToJobsArray(solverState),solverState.jobs._2.toMap)

  Output.saveToFile(schoolSchedule,"schedule.txt")
  Output.printSchedule(schoolSchedule,true)
  ScheduleVisualisation.vis.refresh(schoolSchedule)

  Checker.check(schoolSchedule,solverState.jobs._2.flatMap(x => x._2).toList)

}

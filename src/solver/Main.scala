package solver

object Main extends App {

  val solver = new SolverInterface
  val solverState = solver.getSolverState
  val uiLogic = new UiLogic(solver)

  // PRE-PLACEMENTS
  solver.applyTile("Hana",    (Set(5,6,7,8),  false), MONDAY,    5)
  solver.applyTile("Hana",    (Set(5,6,7,8),  false), MONDAY,    6)
  solver.applyTile("Hana",    (Set(5,6,7,8),  false), THURSDAY,  5)
  solver.applyTile("Hana",    (Set(5,6,7,8),  false), THURSDAY,  6)

  solver.applyTile("Martina", (Set(2,4),      true),  MONDAY,    5)
  solver.applyTile("Martina", (Set(3),        true),  WEDNESDAY, 0)
  solver.applyTile("Martina", (Set(2,4),      true),  FRIDAY,    5)
  solver.applyTile("Martina", (Set(3),        true),  WEDNESDAY, 5)
  solver.applyTile("Martina", (Set(1),        true),  TUESDAY,   5)
  solver.applyTile("Martina", (Set(1),        true),  THURSDAY,  5)

  solver.applyTile("Tereza",  (Set(7,8),      false), TUESDAY,   7)

  solver.applyTile("Lucka",   (Set(7,8),      false), TUESDAY,   6)
  solver.applyTile("Lucka",   (Set(2,4),      false), WEDNESDAY, 5)

  uiLogic.refresh

  Stats.printStats(solverState.tiles,solver.getRowOpen.rows,solverState.jobs._1);

  solver.runRowSolver

  Output.printTiles(solverState)
  uiLogic.refresh

  Stats.printUnplacedTiles(solverState)

  solver.runTilesSolver

  Output.printSchedule(Transformations.solverStateToSchoolSchedule(solverState))
  ScheduleVisualisation.vis.refresh(Transformations.solverStateToSchoolSchedule(solverState))

  val schoolSchedule = Straightener.straighten2(Transformations.solverStateToJobsArray(solverState),solverState.jobs._2.toMap)

  Output.saveToFile(schoolSchedule,"schedule.txt")
  Output.printSchedule(schoolSchedule,true)
  ScheduleVisualisation.vis.refresh(schoolSchedule)

  Checker.check(schoolSchedule,solverState.jobs._2.flatMap(x => x._2).toList)

}

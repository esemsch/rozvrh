package solver

object Main extends App {

  val solverState = TilesSolver.factory

  HardConstraints.applyHardConstraints(solverState)

  val rowOpen = Rows.rowOpenFactory(solverState)

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
    ScheduleVisualisation.vis.refresh(Conversions.tilesToSchoolSchedule(solverState))
  }

  // PRE-PLACEMENTS
  val tilesSolver = solverState.tilesSolver
  val tilesLookup = solverState.tilesLookup
  tilesSolver.applyTile(tilesLookup("Hana")    ((Set(5,6,7,8),  false)), MONDAY,    5)
  tilesSolver.applyTile(tilesLookup("Hana")    ((Set(5,6,7,8),  false)), MONDAY,    6)
  tilesSolver.applyTile(tilesLookup("Hana")    ((Set(5,6,7,8),  false)), THURSDAY,  5)
  tilesSolver.applyTile(tilesLookup("Hana")    ((Set(5,6,7,8),  false)), THURSDAY,  6)

  tilesSolver.applyTile(tilesLookup("Martina") ((Set(2,4),      true)),  MONDAY,    5)
  tilesSolver.applyTile(tilesLookup("Martina") ((Set(3),        true)),  WEDNESDAY, 0)
  tilesSolver.applyTile(tilesLookup("Martina") ((Set(2,4),      true)),  FRIDAY,    5)
  tilesSolver.applyTile(tilesLookup("Martina") ((Set(3),        true)),  WEDNESDAY, 5)
  tilesSolver.applyTile(tilesLookup("Martina") ((Set(1),        true)),  TUESDAY,   5)
  tilesSolver.applyTile(tilesLookup("Martina") ((Set(1),        true)),  THURSDAY,  5)

  tilesSolver.applyTile(tilesLookup("Tereza")  ((Set(7,8),      false)), TUESDAY,   7)

  tilesSolver.applyTile(tilesLookup("Lucka")   ((Set(7,8),      false)), TUESDAY,   6)
  tilesSolver.applyTile(tilesLookup("Lucka")   ((Set(2,4),      false)), WEDNESDAY, 5)

  ScheduleVisualisation.vis.refresh(Conversions.tilesToSchoolSchedule(solverState))

  // PRE-ASSIGNMENTS
  preassignRow(filterRows(TUESDAY,2,List(
    ("Iva",Set(6,7)),
    ("Lucka",Set(8))
  )))
  preassignRow(filterRows(TUESDAY,3,List(
    ("Iva",Set(8)),
    ("Lucka",Set(7))
  )))

  preassignRow(filterRows(WEDNESDAY,2,"Iva",Set(6,7)))
  preassignRow(filterRows(WEDNESDAY,3,List(
    ("Iva",Set(8)),
    ("Lucka",Set(7))
  )))

  preassignRow(filterRows(FRIDAY,4,List(
    ("Hana",Set(2,3,4)),
    ("Bohunka",Set(6,7,8))
  )))

  Stats.printStats(solverState.tiles,rowOpen.rows,solverState.jobs._1);

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
  ScheduleVisualisation.vis.refresh(Conversions.tilesToSchoolSchedule(solverState))

  println(Data.data2.foldLeft(0)((total,j) => total + j.count)-solverState.placed.filter(pl => (pl(2) != -1)).size)
  solverState.tiles.filter(t => solverState.counts(t.id)>0).foreach(t => println(t.job+" --- "+solverState.counts(t.id)))

  rowSolver.maxRowSearch(0,1)

  Output.printSchedule(Conversions.tilesToSchoolSchedule(solverState))
  ScheduleVisualisation.vis.refresh(Conversions.tilesToSchoolSchedule(solverState))

  val schoolSchedule = Straightener.straighten2(Conversions.tilesToJobsArray(solverState),solverState.jobs._2.toMap)

  Output.saveToFile(schoolSchedule,"schedule.txt")
  Output.printSchedule(schoolSchedule,true)
  ScheduleVisualisation.vis.refresh(schoolSchedule)

  Checker.check(schoolSchedule,solverState.jobs._2.flatMap(x => x._2).toList)

}

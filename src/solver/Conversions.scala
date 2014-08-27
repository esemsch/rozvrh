package solver

object Conversions {

  def tilesToJobsArray(solverState:SolverState) = {
    def findJob(grade:Int, day:Int, hour:Int) = {
      val allThatDayAndHour = solverState.placed.filter(pi => {
        (pi(0) == day) && (pi(1) == hour)
      })
      val find = allThatDayAndHour.find(pi => {
        val tile = solverState.tiles(pi(2))
        tile.job.classHour.classes.contains(grade+1)
      })
      find.map(x => solverState.tiles(x(2)).job)
    }

    val sch = new Array[Array[Array[Job]]](LAST_GRADE - FIRST_GRADE + 1)
    (FIRST_GRADE to LAST_GRADE).foreach(gr => {
      sch(gr) = new Array[Array[Job]](FRIDAY - MONDAY + 1)
      (MONDAY to FRIDAY).foreach(d => {
        sch(gr)(d) = new Array[Job](LAST_HOUR - FIRST_HOUR + 1)
        (FIRST_HOUR to LAST_HOUR).foreach(h => {
          sch(gr)(d)(h) = findJob(gr-1,d,h).getOrElse(null)
        })
      })
    })

    sch
  }

  def tilesToSchoolSchedule(solverState:SolverState) = {

    val schoolSchedule = new SchoolSchedule()

    tilesToJobsArray(solverState)
      .zipWithIndex.foreach(gr => gr._1.zipWithIndex.foreach(d => d._1.zipWithIndex.foreach(h => {
      schoolSchedule.schoolSchedule(gr._2).classSchedule(d._2)(h._2) = Option(h._1).flatMap(j => Option(j.toTeachersJob)).getOrElse(null)
    })))

    schoolSchedule
  }

  def teachersJobsArrayToSchoolSchedule(tjs:Array[Array[Array[TeachersJob]]]) = {
    val schoolSchedule = new SchoolSchedule()

    tjs.zipWithIndex.foreach(gr => gr._1.zipWithIndex.foreach(d => d._1.zipWithIndex.foreach(h => {
      schoolSchedule.schoolSchedule(gr._2).classSchedule(d._2)(h._2) = Option(h._1).getOrElse(null)
    })))

    schoolSchedule
  }

}

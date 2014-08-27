package solver

object HardConstraints {

  def applyHardConstraints(solverState:SolverState): Unit = {
    // UNAVAILABILITY
    List(
      Teacher("Iva")     -> (Set(MONDAY,THURSDAY,FRIDAY),     0 to 7),
      Teacher("Iva")     -> (Set(TUESDAY),                   (0 to 1)++(4 to 7)),
      Teacher("Iva")     -> (Set(WEDNESDAY),                 (0 to 1)++(4 to 7)),
      Teacher("Bohunka") -> (Set(TUESDAY,WEDNESDAY,THURSDAY), 0 to 7),
      Teacher("Dana")    -> (Set(TUESDAY),                    0 to 7),
      Teacher("Lucka")   -> (Set(THURSDAY,FRIDAY),            0 to 7),
      Teacher("Tereza")  -> (Set(MONDAY,WEDNESDAY,FRIDAY),    3 to 7),
      Teacher("Tereza")  -> (Set(TUESDAY),                    3 to 6),
      Teacher("Tereza")  -> (Set(THURSDAY),                   0 to 1),
      Teacher("Gita")    -> ((MONDAY to FRIDAY).toSet,        6 to 7), // relax if needed
      Teacher("Hana")    -> (Set(TUESDAY,WEDNESDAY,FRIDAY),   5 to 7)).foreach(ta => {
      val index = solverState.teachers.indexOf(ta._1)
      ta._2._1.foreach(d => {
        ta._2._2.foreach(h => solverState.places(d)(h)(1) = setBit(solverState.places(d)(h)(1),index))
      })
    })

    // FREEHOURS
    def freeHours(days:Seq[Int],grades:Seq[Int],hours:Seq[Int]) {
      days.foreach(d => hours.foreach(h => {
        grades.foreach(gr => {
          solverState.places(d)(h)(0) = setBit(solverState.places(d)(h)(0),(gr))
        })
      }))
    }

    freeHours(MONDAY to FRIDAY,                         FIRST_GRADE to FIRST_GRADE+2,    List(0))
    freeHours(MONDAY to FRIDAY,                         FIRST_GRADE to FIRST_GRADE+2,    6 to 7)
    freeHours(List(FRIDAY),                             List(FIRST_GRADE+1),             5 to 7)
    freeHours((MONDAY to TUESDAY)++(THURSDAY to FRIDAY),FIRST_GRADE+3 to FIRST_GRADE+4,  List(0))
    freeHours(MONDAY to FRIDAY,                         FIRST_GRADE+3 to FIRST_GRADE+4,  List(6))
    freeHours(TUESDAY to FRIDAY,                        FIRST_GRADE+3 to FIRST_GRADE+4,  6 to 7)
    freeHours(List(TUESDAY,WEDNESDAY,FRIDAY),           FIRST_GRADE to LAST_GRADE-2,     6 to 7)
    freeHours(List(WEDNESDAY,FRIDAY),                   List(LAST_GRADE-1,LAST_GRADE),   6 to 7)
    freeHours(List(TUESDAY),                            List(LAST_GRADE-1,LAST_GRADE),   List(5))
    freeHours(List(MONDAY),                             FIRST_GRADE+5 to LAST_GRADE,     List(0))
  }

}

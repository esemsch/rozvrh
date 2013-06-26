package solver

object Conversions {

  def tilesToJobsArray(places:Array[Array[Array[Int]]],tiles:Seq[Tile],placed:Seq[Array[Int]]) = {
    def findJob(grade:Int, day:Int, hour:Int) = {
      val allThatDayAndHour = placed.filter(pi => {
        (pi(0) == day) && (pi(1) == hour)
      })
      val find = allThatDayAndHour.find(pi => {
        val tile = tiles(pi(2))
        tile.job.classHour.classes.contains(grade+1)
      })
      find.map(x => tiles(x(2)).job)
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

  def tilesToSchoolSchedule(places:Array[Array[Array[Int]]],tiles:Seq[Tile],placed:Seq[Array[Int]]) = {

    val schoolSchedule = new SchoolSchedule()

    tilesToJobsArray(places,tiles,placed).zipWithIndex.foreach(gr => gr._1.zipWithIndex.foreach(d => d._1.zipWithIndex.foreach(h => {
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

  def remapGrades(ss:SchoolSchedule) = {
    val schoolSchedule = new SchoolSchedule()

    ss.schoolSchedule.zipWithIndex.foreach(gri => gri._1.classSchedule.zipWithIndex.foreach(di => di._1.zipWithIndex.foreach(hi => {
      val tj = hi._1
      val gr = gri._2
      val d = di._2
      val h = hi._2
      val ntj = if(tj!=null) {
        TeachersJob(tj.teacher,ClassHour(tj.classHour.subject.replace("2","1"),if(tj.classHour.classes.contains(1)) {
          (tj.classHour.classes - 1) + 0
        } else tj.classHour.classes))
      } else null
      schoolSchedule.schoolSchedule(gr).classSchedule(d)(h) = ntj
    })))

    schoolSchedule.schoolSchedule(FIRST_GRADE+1).classSchedule.zipWithIndex.foreach(x => x._1.zipWithIndex.foreach(y => {
      schoolSchedule.schoolSchedule(FIRST_GRADE).classSchedule(x._2)(y._2) = y._1
      schoolSchedule.schoolSchedule(FIRST_GRADE+1).classSchedule(x._2)(y._2) = null
    }))

    schoolSchedule
  }

}

package solver

object Input {

  def readScheduleFromFile(file:String) = {
    val rows =  scala.io.Source.fromFile(file).getLines().toArray
    val ss = new SchoolSchedule
    (MONDAY to FRIDAY).foreach(d => {
      (FIRST_GRADE to LAST_GRADE).foreach(gr => {
        val row = rows(d*9 + gr)
        row.split(",").zipWithIndex.foreach(hi => {
          val h = hi._2
          val in = hi._1
          var tj:TeachersJob = null
          if(!in.isEmpty) {
            val teacher = """\((\S+)\)""".r.findAllIn(in).matchData.next.group(1)
            val ch = Data.createClassHour(in.substring(0,in.indexOf("(")-1))
            tj = TeachersJob(Teacher(teacher),ch)
          }
          ss.schoolSchedule(gr).classSchedule(d)(h) = tj
        })
      })
    })
    ss
  }

}

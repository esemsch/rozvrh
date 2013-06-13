package x

object Conversions {

  def tilesToSchoolSchedule(places:Array[Array[Array[Int]]],tiles:Seq[Tile],placed:Seq[Array[Int]]) = {
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
    def tilesToLine(tile:Array[Int],day:Int,hour:Int) = {
      def isBitThere(pos:Int,int:Int) = {
        val aux = math.pow(2,pos).toInt
        (int & aux) > 0
      }
      (FIRST_GRADE+1 to LAST_GRADE).map(gr => if(isBitThere(gr-1,tile(0))) {
        findJob(gr-1,day,hour) match {
          case None => null
          case Some(job) => job.toTeachersJob
        }
      } else null)
    }
    val schoolSchedule = new SchoolSchedule()

    (MONDAY to FRIDAY).foreach(d => {
      (FIRST_HOUR to LAST_HOUR).foreach(h => {
        val line = tilesToLine(places(d)(h),d,h)
        line.zipWithIndex.foreach(grLine => schoolSchedule.schoolSchedule(grLine._2+1).classSchedule(d)(h) = grLine._1)
      })
    })

    schoolSchedule
  }

}

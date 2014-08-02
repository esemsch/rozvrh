package solver

object Stats  {

  def printStats(tiles:Array[Tile],rows:Array[Array[Int]],jobs:List[Job]) {
    println("Tiles that make no rows: ")
    println (tiles.filter(t => !rows.exists(r => r.exists(tid => tid==t.id))).map(t => t.job).mkString(","))
    println

    println("Number of hours per teacher: ")
    println(jobs.foldLeft(collection.mutable.Map[String,Int]())((m:collection.mutable.Map[String,Int],job:Job) => {
      val hours: Option[Int] = m.get(job.teacher.name)
      m.put(job.teacher.name,job.count + (hours match {
        case None => 0
        case Some(i) => i
      }))
      m
    }).map(e => e._1+" --- "+e._2).mkString("\n"));
    println

    println("Number of hours per class: ")
    println(jobs.foldLeft(collection.mutable.Map[Int,Int]())((m:collection.mutable.Map[Int,Int],job:Job) => {
      job.classHour.classes.foreach(c => {
        val hours: Option[Int] = m.get(c)
        m.put(c,job.count + (hours match {
          case None => 0
          case Some(i) => i
        }))
      })
      m
    }).map(e => e._1+1+" --- "+e._2).toList.sorted.mkString("\n"));
    println

  }


}

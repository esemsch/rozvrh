package solver

import scala.collection.mutable.Map

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
    }
    ).map(e => e._1+" --- "+e._2).mkString("\n"));
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
    }
    ).map(e => e._1+1+" --- "+e._2).toList.sorted.mkString("\n"));
    println

    println("Number of hours per class and subject: ")
    println(jobs.foldLeft(Map[Int,Map[String,Int]]())((m:Map[Int,Map[String,Int]],job:Job) => {
      val subjects: String = job.classHour.subjects.mkString(",")
      job.classHour.classes.foreach(c => {
        val classSbjs: Option[Map[String,Int]] = m.get(c)
        classSbjs match {
          case None => {
            val sbjMap = Map[String,Int]((subjects -> job.count))
            m.put(c,sbjMap)
          }
          case Some(csbj) => {
            val sbjs = csbj.get(subjects)
            sbjs match {
              case None => csbj.put(subjects,job.count)
              case Some(i) => csbj.put(subjects,job.count+i)
            }
          }
        }
      })
      m
    }
    ).flatMap(e => e._2.map(e2 => (e._1+1)+": "+e2._1+" --- "+e2._2)).toList.sorted.mkString("\n"));

    println


  }

  def printTilesToGo(solverState:SolverState): Unit = {
    println("Number of tiles to place: "+(Data.data2.foldLeft(0)((total,j) => total + j.count)-solverState.placed.filter(pl => (pl(2) != -1)).size))
    solverState.tiles.filter(t => solverState.counts(t.id)>0).foreach(t => println(t.job+" --- "+solverState.counts(t.id)))
  }


}

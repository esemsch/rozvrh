package x

import scala.collection.mutable

object Straightener {

  def straighten(schedule:Array[Array[Array[Job]]],realJobs:Map[Job,List[TeachersJob]]) = {
    val schoolSchedule = new SchoolSchedule()

    def strInner(str:(Int,Int,Int,Job)=>Unit) {
      (FIRST_GRADE to LAST_GRADE).foreach(gr => {
        (MONDAY to FRIDAY).foreach(d => {
          (FIRST_HOUR to LAST_HOUR).foreach(h => {
            Option(schedule(gr)(d)(h)) match {
              case Some(j) => {
                if(!Option(schoolSchedule.schoolSchedule(gr).classSchedule(d)(h)).isDefined) {
                  str(gr,d,h,j)
                }
              }
              case None => schoolSchedule.schoolSchedule(gr).classSchedule(d)(h) = null
            }
          })
        })
      })
    }

    val stillAvailable = new mutable.HashMap[Job,List[TeachersJob]]() ++ realJobs.map(x => (x._1,x._2))
    val alreadyPlaced = new mutable.HashMap[Job,List[(TeachersJob,Int,Int)]]()
    strInner((gr:Int,d:Int,h:Int,j:Job) => {
      val placed = alreadyPlaced.get(j).getOrElse(Nil)
      val available = stillAvailable(j).sortBy(tj => {
        def main = {
          if(h>=1 && h<=4 && tj.classHour.mainSubject) -1 else 0
        }
        def nonRep = {
          placed.filter(x => x._1==tj && x._2==d).size
        }
        main + nonRep
      })
      val tj = available.head
      tj.classHour.classes.foreach(rgr => schoolSchedule.schoolSchedule(rgr).classSchedule(d)(h) = tj)
      stillAvailable += (j -> (available diff List(tj)))
      alreadyPlaced += (j -> ((tj,d,h) :: placed))
    })

    schoolSchedule
  }

}

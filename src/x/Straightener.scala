package x

import scala.collection.mutable
import x.Y.Possibility

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

  def straighten2(schedule:Array[Array[Array[Job]]],realJobs:Map[Job,List[TeachersJob]]) = {
    val possSort = (p1:Possibility,p2:Possibility) => (p1.day<p2.day) || (p1.day==p2.day && p1.hour<p2.hour)

    val jobToTJAndPoss = new mutable.HashMap[Job,(List[TeachersJob],Set[Possibility])]()
    schedule.zipWithIndex.foreach(gr =>
      gr._1.zipWithIndex.foreach(d =>
        d._1.zipWithIndex.foreach(h =>
            Option(schedule(gr._2)(d._2)(h._2)) match {
              case None => {}
              case Some(j) => {
                jobToTJAndPoss.get(j) match {
                  case Some(e) => jobToTJAndPoss += (j -> (realJobs(j),e._2 + Possibility(d._2,h._2)))
                  case None => jobToTJAndPoss += (j -> (realJobs(j),Set(Possibility(d._2,h._2))))
                }
              }
            }
        )
      )
    )

    val schoolSchedule = new SchoolSchedule

    jobToTJAndPoss.filter(x => x._1.classHour.arts && (2 <= x._2._1.foldLeft(0)((tot,y) => tot + (if(y.classHour.arts) 1 else 0)))).foreach(x => {
      val artTjs = x._2._1.filter(y => y.classHour.arts)
      var remPoss = List[Possibility]()
      x._2._2.toList.sortWith(possSort).foldLeft(Possibility(-10,-10))((l,p) => {
        if(l.day == p.day && l.hour == (p.hour-1) && remPoss.isEmpty) {
          artTjs(0).classHour.classes.foreach(rgr => schoolSchedule.schoolSchedule(rgr).classSchedule(l.day)(l.hour) = artTjs(0))
          artTjs(1).classHour.classes.foreach(rgr => schoolSchedule.schoolSchedule(rgr).classSchedule(p.day)(p.hour) = artTjs(1))
          remPoss = l :: p :: remPoss
        }
        p
      })
      if(!remPoss.isEmpty) {
        jobToTJAndPoss += (x._1 -> (x._2._1 diff artTjs, x._2._2 -- remPoss.toSet))
      }
    })

    jobToTJAndPoss.filter(x => x._1.classHour.twoHour).foreach(x => {
      val twoHourMap = new mutable.HashMap[Set[String],List[TeachersJob]]()
      x._2._1.filter(y => y.classHour.twoHour).foreach(y => {
        twoHourMap.get(y.classHour.subjects) match {
          case None => twoHourMap += (y.classHour.subjects -> (y :: Nil))
          case Some(tjs) => twoHourMap += (y.classHour.subjects -> (y :: tjs))
        }
      })
      var remPoss = List[(TeachersJob,Possibility)]()
      twoHourMap.filter(y => (y._2.size >= 2)).foreach(y => {
        x._2._2.toList.sortWith(possSort).foldLeft(Possibility(-10,-10))((l,p) => {
          if(l.day == -10) p
          else if ((p.day - l.day) >= 2) {
            y._2(0).classHour.classes.foreach(rgr => schoolSchedule.schoolSchedule(rgr).classSchedule(l.day)(l.hour) = y._2(0))
            y._2(1).classHour.classes.foreach(rgr => schoolSchedule.schoolSchedule(rgr).classSchedule(p.day)(p.hour) = y._2(1))
            remPoss = (y._2(0),l) :: (y._2(1),p) :: remPoss
            Possibility(-10,-10)
          } else l
        })
      })
      if(!remPoss.isEmpty) {
        jobToTJAndPoss += (x._1 -> (x._2._1 diff remPoss.unzip._1, x._2._2 -- remPoss.unzip._2.toSet))
      }
    })

    jobToTJAndPoss.unzip._2.foreach(x => {
      var tjs = x._1
      val poss = x._2.toList.sortWith(possSort)
      val tjsByDays = mutable.Map[Int,List[TeachersJob]]()
      val hoursAvailPerDay = new Array[Int](FRIDAY - MONDAY + 1)
      poss.foreach(p => hoursAvailPerDay(p.day)=hoursAvailPerDay(p.day)+1)
      (MONDAY to FRIDAY).filter(x => hoursAvailPerDay(x)>0).sortBy(x => -hoursAvailPerDay(x)).foreach(d => {
        (0 to hoursAvailPerDay(d)-1).foreach(h => {
          val uniques = tjs.toSet.toList
          val tj = uniques(h%uniques.size)
          tjs = tjs diff List(tj)
          tjsByDays.get(d) match {
            case None => tjsByDays += (d -> (tj :: Nil))
            case Some(l) => tjsByDays += (d -> (tj :: l))
          }
        })
      })
      tjsByDays.foreach(e => {
        val d = e._1
        val tjs = e._2
        val possFilt = poss.filter(p => p.day == d)
        tjs.zipWithIndex.foreach(tj => {
          val p = possFilt(tj._2)
          tj._1.classHour.classes.foreach(rgr => schoolSchedule.schoolSchedule(rgr).classSchedule(p.day)(p.hour) = tj._1)
        })
      })
    })

    schoolSchedule
  }

}

package solver

import scala.collection.mutable

object Straightener {

  def straighten2(schedule:Array[Array[Array[Job]]],realJobs:Map[Job,List[TeachersJob]],preStraightenings:Map[Job,List[(Int,Int,String)]] = Map()) = {
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

    preStraightenings.foreach(x => {
      val job = x._1
      x._2.foreach(y => {
        val d = y._1
        val h = y._2
        val subj = y._3
        val e = jobToTJAndPoss(job)
        val tj = e._1.find(tjx => tjx.classHour.subjects.contains(subj)).get
        tj.classHour.classes.foreach(ci => schoolSchedule.schoolSchedule(ci).classSchedule(d)(h) = tj)

        jobToTJAndPoss += (job -> (e._1 diff List(tj), e._2 - Possibility(d, h)))
      })
    })

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

    val twoArtsDifferentClasses = jobToTJAndPoss.filter(_._1.classHour.arts).map(x => {
      val filtered = jobToTJAndPoss.filter(_._1.classHour.arts).filter(y => {
        x!=y && (x._1.classHour.classes diff y._1.classHour.classes).size < x._1.classHour.classes.size
      })
      if(filtered.isEmpty) (x, List[(Job, (List[TeachersJob], Set[Possibility]))]())
      else (x, filtered.toList)
    }).filter(x => !x._2.isEmpty)

    if(!twoArtsDifferentClasses.isEmpty) {
      twoArtsDifferentClasses.foldLeft(List(twoArtsDifferentClasses.head))((l,ths) => {
        if(!l.contains(ths) && !l.exists(x => x._2.exists(y => y == ths._1))) ths :: l
        else l
      }).foreach(x => {
        val a1 = x._1
        val a1tj = x._1._2._1.find(_.classHour.arts).get
        val a2l = x._2

        a2l.exists(a2 => {
          val a2tj = a2._2._1.find(_.classHour.arts).get
          a1._2._2.exists(p1 => {
            a2._2._2.exists(p2 => {
              if(p1.day == p2.day && math.abs(p1.hour-p2.hour)==1) {
                a1tj.classHour.classes.foreach(rgr => schoolSchedule.schoolSchedule(rgr).classSchedule(p1.day)(p1.hour) = a1tj)
                a2tj.classHour.classes.foreach(rgr => schoolSchedule.schoolSchedule(rgr).classSchedule(p2.day)(p2.hour) = a2tj)
                jobToTJAndPoss += (a1._1 -> ((a1._2._1 diff List(a1tj),a1._2._2 - p1)))
                jobToTJAndPoss += (a2._1 -> ((a2._2._1 diff List(a2tj),a2._2._2 - p2)))
                true
              } else false
            })
          })
        })
      })
    }

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
          val minHour = poss.filter(p => p.day == d).foldLeft(100)((min,p) => if(min>p.hour)p.hour else min)
          val uniques = tjs.toSet.toList.sortWith((tj1,tj2) => if(minHour>5){
            !tj1.classHour.mainSubject
          } else tj1.classHour.mainSubject)
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
        val tjs = e._2.reverse
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

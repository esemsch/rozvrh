package x

object X extends App {

  var (teachersJobs,teachers) = Data.data

  val schoolSchedule: SchoolSchedule = new SchoolSchedule()

  // <preassignment>
  def preassign(tj:TeachersJob,day:Int,hour:Int):Unit = {
    if(!(teachersJobs.contains(tj))) throw new RuntimeException("Preassigning non-existent TJ")
    teachersJobs = teachersJobs diff List(tj)
//    tj.classHour.classes.foreach(c => schoolSchedule.schoolSchedule(c).classSchedule(day)(hour) = tj)
  }

  preassign(TeachersJob(Teacher("Iva"),ClassHour("Rj 7",Set(6))),FRIDAY,2)
  preassign(TeachersJob(Teacher("Iva"),ClassHour("Rj 7",Set(6))),FRIDAY,3)

  val secondary = Set(5,6,7,8)

  preassign(TeachersJob(Teacher("Lucka"),ClassHour("Tv_Dív 6/7/8/9",secondary)),MONDAY,5)
  preassign(TeachersJob(Teacher("Lucka"),ClassHour("Tv_Dív 6/7/8/9",secondary)),THURSDAY,6)
  preassign(TeachersJob(Teacher("Lucka"),ClassHour("Tv_Chl 6/7/8/9",secondary)),MONDAY,6)
  preassign(TeachersJob(Teacher("Lucka"),ClassHour("Tv_Chl 6/7/8/9",secondary)),THURSDAY,5)

  preassign(TeachersJob(Teacher("Tereza"),ClassHour("Vv 6/7",Set(5,6))),MONDAY,4)
  preassign(TeachersJob(Teacher("Tereza"),ClassHour("Vv 6/7",Set(5,6))),MONDAY,7)
  preassign(TeachersJob(Teacher("Tereza"),ClassHour("Vv 8/9",Set(7,8))),THURSDAY,7)
  // </preassignment>

  // <subject groups>
  val combinedComparator: (TeachersJob, TeachersJob) => Boolean = (tj1, tj2) => (tj1.classHour.combinedClasses && !tj2.classHour.combinedClasses)

  val hlavniPredmetyTJ = teachersJobs.filter(tj => tj.classHour.mainSubject).sortWith(combinedComparator)
  val peTJ = teachersJobs.filter(tj => tj.classHour.pe).sortWith(combinedComparator)
  val dvojhodinoveTJ = (teachersJobs.filter(tj => tj.classHour.twoHour) diff hlavniPredmetyTJ).sortWith(combinedComparator)
  val vvTJ = teachersJobs.filter(tj => tj.classHour.arts).sortWith(combinedComparator)
  val ostatniTJ: List[TeachersJob] = (teachersJobs diff hlavniPredmetyTJ diff peTJ diff dvojhodinoveTJ diff vvTJ).sortWith(combinedComparator)


  val subjectGroups: List[List[TeachersJob]] = List(hlavniPredmetyTJ, peTJ, dvojhodinoveTJ, vvTJ)
  println(teachersJobs.size)
  println(subjectGroups.foldLeft(0)((total,l) => total + l.size))
  println(ostatniTJ.size)
  println(ostatniTJ)

  subjectGroups.combinations(2).foreach(c => {
    def cdiff(i:Int,j:Int) {
      val s1: Int = c(i).size
      val d1: List[TeachersJob] = c(i) diff c(j)
      val sd1: Int = d1.size
      if(s1!=sd1) {
        println(c(i) diff d1)
      }
    }
    cdiff(0,1)
    cdiff(1,0)
  })
  // <subject groups>

  // <teacher availability>
  val teachersAvailability = (MONDAY to FRIDAY).map(i => teachers.filter(t => {
    if(t == Teacher("Bohunka") && !Set(MONDAY,FRIDAY).contains(i)) false
    else if(t == Teacher("Eva") && !Set(MONDAY,WEDNESDAY,THURSDAY).contains(i)) false
    else if(t == Teacher("Lucka") && !Set(MONDAY,TUESDAY,WEDNESDAY,THURSDAY).contains(i)) false
    else true
  }))

  (MONDAY to FRIDAY).foreach(d => println(d+": "+teachersAvailability(d).size))
  // <teacher availability>

  // <subject groups by classes>
  def byClasses(tjs:Seq[TeachersJob]) = {
    (FIRST_GRADE to LAST_GRADE).map(cls => tjs.filter(tj => tj.classHour.classes.contains(cls)))
  }

  val hlavniPredmetyTJByClasses = byClasses(hlavniPredmetyTJ)
  val peTJByClasses = byClasses(peTJ)
  val dvojhodinoveTJByClasses = byClasses(dvojhodinoveTJ)
  val vvTJByClasses = byClasses(vvTJ)
  val ostatniTJByClasses = byClasses(ostatniTJ)

  def printPredmetyByClasses(a:Seq[Seq[TeachersJob]]) {
    a.zipWithIndex.foreach(x => println((x._2+1)+". Trida: "+x._1))
    println("")
  }
  printPredmetyByClasses(hlavniPredmetyTJByClasses)
  printPredmetyByClasses(peTJByClasses)
  printPredmetyByClasses(dvojhodinoveTJByClasses)
  printPredmetyByClasses(vvTJByClasses)
  printPredmetyByClasses(ostatniTJByClasses)
  // </subject groups by classes>

  // <schedule>
  def schedule(tj:TeachersJob,hoursToSch:Seq[Int],day:Int,withReschedule:Boolean = false) = {

    def checkAssgnOk(tj:TeachersJob,day:Int,hour:Int) = {
      if(!teachersAvailability(day).contains(tj.teacher) ||
        (tj.classHour.firstSecond && (hour == 0 || hour > 5)) ||
        (tj.classHour.third && (hour > 5))) false
      else {
        val free = tj.classHour.classes.forall(cls => schoolSchedule.schoolSchedule(cls).classSchedule(day)(hour)==null)
        val nonExcluded = (FIRST_GRADE to LAST_GRADE).forall(gr => {
          val aux = schoolSchedule.schoolSchedule(gr).classSchedule(day)(hour)
          aux == null || aux.teacher != tj.teacher
        })
        free && nonExcluded
      }
    }

    def scheduleForHours(hours:Seq[Int]) = {
      hours.exists(h => {
        if(!(day == MONDAY || day == THURSDAY) && h>5) false
        else {
          val ok = checkAssgnOk(tj,day,h)
          if(ok) {
            tj.classHour.classes.foreach(cls =>schoolSchedule.schoolSchedule(cls).classSchedule(day)(h) = tj)
          }
          ok
        }
      })
    }

    def reSchedule = {
      if(tj.classHour.combinedClasses) false
      else {
        val gr = tj.classHour.classes.head
        val freeHours = schoolSchedule.schoolSchedule(gr).classSchedule.zipWithIndex.flatMap(ds => ds._1.zipWithIndex.filter({
          x => (x._1==null && hoursToSch.contains(x._2) && ((ds._2 == MONDAY || ds._2 == THURSDAY) || x._2<=5))
        }).map(x => (ds._2,x._2)))
        val toSwapCandidates = schoolSchedule.schoolSchedule(gr).classSchedule.zipWithIndex.flatMap(ds => ds._1.zipWithIndex.filter({
          x => x._1!=null && !x._1.classHour.combinedClasses
        }).map(x => (ds._2,x._2)))

        def trySwap(candidate:(Int,Int),freeHour:(Int,Int)) = {
          val ctj = schoolSchedule.schoolSchedule(gr).classSchedule(candidate._1)(candidate._2)
          if(checkAssgnOk(ctj,freeHour._1,freeHour._2)) {
            schoolSchedule.schoolSchedule(gr).classSchedule(candidate._1)(candidate._2) = null
            if (checkAssgnOk(tj,candidate._1,candidate._2)) {
              schoolSchedule.schoolSchedule(gr).classSchedule(freeHour._1)(freeHour._2) = ctj
              schoolSchedule.schoolSchedule(gr).classSchedule(candidate._1)(candidate._2) = tj
              true
            } else {
              schoolSchedule.schoolSchedule(gr).classSchedule(candidate._1)(candidate._2) = ctj
              false
            }
          } else false
        }

        toSwapCandidates.exists(cand => {
          freeHours.exists(fh => {
            trySwap(cand,fh)
          })
        })
      }
    }

    if (!scheduleForHours(hoursToSch) && withReschedule) {
      reSchedule
    } else true
  }

  def scheduleMainSubjects(subjGr:Seq[TeachersJob],prefHours:Seq[Int]) = {
    subjGr.zipWithIndex.filter(tji => {
      val day = tji._2 % 5
      !((day to (day+5)).exists(d => {
        val daux = d % 5
        schedule(tji._1,prefHours,daux)
      }))
    }).map(tji => tji._1)
  }

  def scheduleTwoHoursSubjects(twoHours:Seq[TeachersJob],prefHours:Seq[Int]) = {
    def isConsequent(tj1:TeachersJob,tj2:TeachersJob) = {
      ((tj1.classHour.classes diff tj2.classHour.classes).size == 0 || (tj2.classHour.classes diff tj1.classHour.classes).size == 0) &&
      ((tj1.classHour.subjects diff tj2.classHour.subjects).size < tj1.classHour.subjects.size)
    }
    val precedence = twoHours.foldLeft(List[(TeachersJob,Seq[TeachersJob])]())((l,tj) => l.find(precJobs => isConsequent(precJobs._1,tj)) match {
      case None => l ++ List((tj,List()))
      case Some(tj1) => {
        val newTj1 = (tj1._1,tj1._2 ++ Set(tj))
        (l diff List(tj1)) ++ List(newTj1)
      }
    })
    def scheduleTwo(prec:(TeachersJob,Seq[TeachersJob]),d:Int) = {
      val mainOk = schedule(prec._1,prefHours,d)
      if(mainOk) {
        prec._2.filter(tj => schedule(tj,prefHours,(d+2)%5,false))
      } else List(prec._1) ++ prec._2
    }
    precedence.zipWithIndex.flatMap(tji => {
      val day = tji._2 % 5
      val rests: (List[TeachersJob], Boolean) = (day to (day + 5)).foldLeft((List[TeachersJob](), false))((result, d) => {
        if (!result._2) {
          val daux = d % 5
          val rest = scheduleTwo(tji._1, daux)
          if (rest.size < 1 + tji._1._2.size) (rest.toList, true) else result
        } else result
      })
      rests._1
    })
  }

  def teachersComparator(tj1:TeachersJob,tj2:TeachersJob) = {
    val teachersSorted: List[Teacher] = teachers.toList.sortWith((t1,t2)=>t1.name.compareTo(t2.name)<=0)
    teachersSorted.foldLeft(tj1)((tj, tjt) => if (tjt == tj1.teacher) tj1 else if (tjt == tj2.teacher) tj2 else tj) == tj2
  }

  val rest1 = scheduleMainSubjects(hlavniPredmetyTJ.filter(_.classHour.combinedClasses).sortWith(teachersComparator(_,_)),(1 to 4))
//  val rest2 = scheduleMainSubjects(peTJ.filter(_.classHour.combinedClasses),(5 to 7))
  val rest3 = scheduleTwoHoursSubjects(dvojhodinoveTJ.sortWith(teachersComparator(_,_)),(0 to 5))
//  val rest4 = scheduleMainSubjects(vvTJ.filter(_.classHour.combinedClasses),(5 to 7))
//  val rest5 = scheduleMainSubjects(ostatniTJ.filter(_.classHour.combinedClasses).sortWith(teachersComparator(_,_)),(1 to 4))

  val rest12 = scheduleMainSubjects(hlavniPredmetyTJ.filter(!_.classHour.combinedClasses).sortWith(teachersComparator(_,_)),(1 to 4))
//  val rest22 = scheduleMainSubjects(peTJ.filter(!_.classHour.combinedClasses),(5 to 7))
//  val rest42 = scheduleMainSubjects(vvTJ.filter(!_.classHour.combinedClasses),(5 to 7))
//  val rest52 = scheduleMainSubjects(ostatniTJ.filter(!_.classHour.combinedClasses).sortWith(teachersComparator(_,_)),(1 to 4))

//  val rests: Seq[TeachersJob] = rest1 ++ rest3 ++ rest5 ++ rest12 ++ rest32 ++ rest52 ++ rest2 ++ rest4 ++ rest22 ++ rest42
//  scheduleMainSubjects(rests,(0 to 7))
//  println(rests)

  Output.printSchedule(schoolSchedule)

  println(rest1++rest12++rest3)
//  println(rest1++rest3++rest5++rest12++rest32++rest52)

//  val auxJobsByTeachers = (teachers.map(t => List(t.name) ++ teachersJobs.filter(tj => tj.teacher == t).map(_.toString).toList).toList)
//  val maxSize = auxJobsByTeachers.foldLeft(0)((max,l) => if(max > l.size) max else l.size )
//  Output.printTable(auxJobsByTeachers.map(l => l.padTo(maxSize,null)),false)

  Checker.check(schoolSchedule,teachersJobs)
}

package x

object X extends App {

  def createTeachersJobs(subject:String,count:Int,teacher:String) = {
    val clss = subject.filter(_.isDigit).map(_.toString.toInt-1).toSet
    val ch = new ClassHour(subject,clss)
    (1 to count).map(i => new TeachersJob(Teacher(teacher),ch)).toList
  }

  val TerezaJobs = Map(
    "Aj 8/9" -> 1,
    "Aj 8" -> 2,
    "Aj 9" -> 2,
    "Aj 6/7" -> 1,
    "Aj 6" -> 2,
    "Aj 7" -> 2,
    "Vl 4/5" -> 1,
    "Vl 4" -> 1,
    "Vl 5" -> 1,
    "Prv 3/Př 4" -> 2,
    "Prv 3" -> 1,
    "Vv 2/4" -> 1,
    "Vv 4" -> 1,
    "Hv 3/5" -> 1,
    "Vv 6/7" -> 2,
    "Vv 8/9" -> 1
  ).flatMap(e => createTeachersJobs(e._1,e._2,"Tereza")).toList

  val AlenaJobs = Map(
    "F/M 6/7" -> 1,
    "M/F 6/7" -> 1,
    "F 6" -> 1,
    "F 7" -> 1,
    "M 6" -> 3,
    "M 7" -> 3,
    "M 6/7" -> 1,
    "F/M 8/9" -> 1,
    "M/F 8/9" -> 1,
    "M 8" -> 3,
    "M 9" -> 3,
    "M 8/9" -> 1,
    "F 8" -> 1,
    "F 9" -> 1
  ).flatMap(e => createTeachersJobs(e._1,e._2,"Alena")).toList

  val HanaJobs = Map(
    "Čj 6/7" -> 1,
    "Čj 6" -> 3,
    "Čj 7" -> 3,
    "D 6/7" -> 1,
    "D 6" -> 1,
    "D 7" -> 1,
    "D 8" -> 2,
    "D 9" -> 2,
    "Z 6/7" -> 1,
    "Z 6" -> 1,
    "Z 7" -> 1,
    "Z 8/9" -> 1,
    "Z 8" -> 1,
    "Z 9" -> 1,
    "Hv 6/7" -> 1,
    "Hv 8/9" -> 1
  ).flatMap(e => createTeachersJobs(e._1,e._2,"Hana")).toList

  val GitaJobs = Map(
    "Čj 2/4" -> 8,
    "Čj 2" -> 2,
    "M 2/4" -> 5,
    "Prv/Př  2/4" -> 1,
    "Prv 2" -> 1,
    "Hv 2/4" -> 1,
    "Tv 2/4" -> 2,
    "Pč 2/4" -> 1
  ).flatMap(e => createTeachersJobs(e._1,e._2,"Gita")).toList

  val MartinaJobs = Map(
    "Čj 3/5" -> 7,
    "Čj 3" -> 2,
    "M 3/5" -> 5,
    "Aj 4/5" -> 3,
    "Aj 3" -> 3,
    "Vv 3/5" -> 1,
    "Vv 5" -> 1
  ).flatMap(e => createTeachersJobs(e._1,e._2,"Martina")).toList

  val BohunkaJobs = Map(
    "Čj 8/9" -> 1,
    "Čj 8" -> 3,
    "Čj 9" -> 3,
    "SPV 9" -> 1
  ).flatMap(e => createTeachersJobs(e._1,e._2,"Bohunka")).toList

  val LuckaJobs = Map(
    "Př 8/9" -> 1,
    "Př 8" -> 1,
    "Př 9" -> 1,
    "Ch 8/9" -> 1,
    "Ch 8" -> 1,
    "Ch 9" -> 1,
    "Př 6/7" -> 1,
    "Př 6" -> 1,
    "Př 7" -> 1,
    "Vo 6/7" -> 1,
    "Rv 7" -> 1,
    "Tv 3/5" -> 2,
    "Tv_Dív 6/7/8/9" -> 2,
    "Tv_Chl 6/7/8/9" -> 2
  ).flatMap(e => createTeachersJobs(e._1,e._2,"Lucka")).toList

  val EvaJobs = Map(
    "Inf 5/6" -> 1,
    "Inf 8" -> 2,
    "Inf 9" -> 2,
    "Spv 8" -> 1,
    "Vo 8/9" -> 1,
    "Rv 8/9" -> 1,
    "Pč 3/5" -> 1,
    "Pč 6/7" -> 1,
    "Pč 8/9" -> 1
  ).flatMap(e => createTeachersJobs(e._1,e._2,"Eva")).toList

  val IvaJobs = Map(
    "Rj 7" ->	2
  ).flatMap(e => createTeachersJobs(e._1,e._2,"Iva")).toList

  val teachers = Set("Tereza","Alena","Hana","Gita","Martina","Bohunka","Lucka","Eva","Iva").map(n => Teacher(n))

  var teachersJobs = (TerezaJobs ++ AlenaJobs ++ HanaJobs ++ GitaJobs ++ MartinaJobs ++ BohunkaJobs ++ LuckaJobs ++ EvaJobs ++ IvaJobs)

  val schoolSchedule: SchoolSchedule = new SchoolSchedule()

  val odpol = new OdpoledniVUrciteDny(schoolSchedule,Set(MONDAY,THURSDAY))
  val volna = new VolnaHodina(schoolSchedule)
  val prvniDruha = new PrvniDruha(schoolSchedule)
  val neprerusene = new NepreruseneVyucovaniDopoledne(schoolSchedule)
  val exklusivitaUcitele = new JedenUcitelJednaHodina(schoolSchedule)
  val vvVzdyPoSobe = new VvVzdyPoSobe(schoolSchedule)
  val dvojHodinoveNePoSobe = new DvojhodinnovePredmetyNeVeDnechPoSobe(schoolSchedule)
  val stejnyNeVeStejnyDen = new StejnyPredmetNeVTenSamyDen(schoolSchedule)
  val hlavniPredmety = new HlavniPredmetyRano(schoolSchedule)
  val spojenePredmety = new SpojenePredmetyRano(schoolSchedule)
  val spravnePrirazene = new PredmetySpravnePrirazeneTridam(schoolSchedule)

  val reditelUciJenVPoAPa = new UcitelUciVUrciteDny(schoolSchedule,Teacher("Bohunka"),Set(MONDAY,FRIDAY))
  val evaVolnoVUtAPa = new UcitelUciVUrciteDny(schoolSchedule,Teacher("Eva"),Set(MONDAY,WEDNESDAY,THURSDAY))
  val luckaVolnoVPa = new UcitelUciVUrciteDny(schoolSchedule,Teacher("Lucka"),Set(MONDAY,TUESDAY,WEDNESDAY,THURSDAY))
  val druzinarkaHana = new Druzinar(schoolSchedule,Teacher("Hana"),4)

  // <preassignment>
  def preassign(tj:TeachersJob,day:Int,hour:Int):Unit = {
    if(!(teachersJobs.contains(tj))) throw new RuntimeException("Preassigning non-existent TJ")
    teachersJobs = teachersJobs diff List(tj)
    tj.classHour.classes.foreach(c => schoolSchedule.schoolSchedule(c).classSchedule(day)(hour) = tj)
  }

  preassign(IvaJobs(0),FRIDAY,2)
  preassign(IvaJobs(1),FRIDAY,3)

  val secondary = Set(5,6,7,8)

  preassign(TeachersJob(Teacher("Lucka"),ClassHour("Tv_Dív 6/7/8/9",secondary)),MONDAY,5)
  preassign(TeachersJob(Teacher("Lucka"),ClassHour("Tv_Dív 6/7/8/9",secondary)),THURSDAY,6)
  preassign(TeachersJob(Teacher("Lucka"),ClassHour("Tv_Chl 6/7/8/9",secondary)),MONDAY,6)
  preassign(TeachersJob(Teacher("Lucka"),ClassHour("Tv_Chl 6/7/8/9",secondary)),THURSDAY,5)

  preassign(TeachersJob(Teacher("Tereza"),ClassHour("Vv 6/7",Set(5,6))),MONDAY,5)
  preassign(TeachersJob(Teacher("Tereza"),ClassHour("Vv 6/7",Set(5,6))),MONDAY,7)
  preassign(TeachersJob(Teacher("Tereza"),ClassHour("Vv 8/9",Set(7,8))),THURSDAY,7)
  // </preassignment>

  def printSchedule(s:SchoolSchedule) {

    def printTable(cs:Array[Array[TeachersJob]],leftAxisDays:Boolean) {
      val leftAxisPad = if(leftAxisDays) 10 else 3

      val colLengths = cs.map(ds => ds.map(tj => if(tj==null) 1 else tj.toString.length)).foldLeft((FIRST_HOUR to LAST_HOUR).map(x=>0))((lengths,dayLengths) => {
        lengths.zip(dayLengths).map(x => math.max(x._1,x._2))
      }).toList

      def divide {
        print("".padTo(leftAxisPad+4,"-").mkString)
        colLengths.foreach(cl => print("-"+"".padTo(cl,"-").mkString+"--"))
        print("\n")
      }

      def printLine(ds:Seq[AnyRef],leftAxis:String) {
        print("| "+leftAxis.padTo(leftAxisPad," ").mkString+" | ")
        ds.zip(colLengths).foreach(x => {
          val tj: String = if (x._1==null) "" else x._1.toString
          print(tj.padTo(x._2," ").mkString+" | ")
        })
        print("\n")
      }

      divide

      printLine((FIRST_HOUR to LAST_HOUR).map(_.toString).toList,"")

      divide

      cs.zipWithIndex.foreach(ds => {
        printLine(ds._1,if(leftAxisDays) DAY_NAME(ds._2) else (ds._2+1).toString+".")
        divide
      })
    }

    def printClassSchedule(cs:ClassSchedule) {
      printTable(cs.classSchedule,true)
    }

    (FIRST_GRADE to LAST_GRADE).foreach(gr => {
      println((gr+1)+". Třída")
      printClassSchedule(schoolSchedule.schoolSchedule(gr))
    })

    val byDays = (MONDAY to FRIDAY).map(d => (FIRST_GRADE to LAST_GRADE).map(gr => schoolSchedule.schoolSchedule(gr).classSchedule(d)).toArray)
    byDays.zipWithIndex.foreach(daySch => {
      println(DAY_NAME(daySch._2))
      printTable(daySch._1,false)
    })

  }

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
  def schedule(tj:TeachersJob,hoursToSch:Seq[Int],day:Int) = {

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
          x => (x._1==null && ((ds._2 == MONDAY || ds._2 == THURSDAY) || x._2<=5))
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

    if (!scheduleForHours(hoursToSch)) {
      reSchedule
    } else true
  }

  def scheduleSubjectGroup(subjGr:Seq[TeachersJob],prefHours:Seq[Int]) = {
    subjGr.zipWithIndex.filter(tji => {
      val day = tji._2 % 5
      !((day to (day+5)).exists(d => {
        val daux = d % 5
        schedule(tji._1,prefHours,daux)
      }))
    }).map(tji => tji._1)
  }

  val rest1 = scheduleSubjectGroup(hlavniPredmetyTJ.filter(_.classHour.combinedClasses),(1 to 5))
  val rest2 = scheduleSubjectGroup(peTJ.filter(_.classHour.combinedClasses),(5 to 5))
  val rest3 = scheduleSubjectGroup(dvojhodinoveTJ.filter(_.classHour.combinedClasses),(0 to 7))
  val rest4 = scheduleSubjectGroup(vvTJ.filter(_.classHour.combinedClasses),(5 to 7))
  val rest5 = scheduleSubjectGroup(ostatniTJ.filter(_.classHour.combinedClasses),(0 to 7))

  val rest12 = scheduleSubjectGroup(hlavniPredmetyTJ.filter(!_.classHour.combinedClasses),(1 to 5))
  val rest22 = scheduleSubjectGroup(peTJ.filter(!_.classHour.combinedClasses),(5 to 5))
  val rest32 = scheduleSubjectGroup(dvojhodinoveTJ.filter(!_.classHour.combinedClasses),(0 to 7))
  val rest42 = scheduleSubjectGroup(vvTJ.filter(!_.classHour.combinedClasses),(5 to 7))
  val rest52 = scheduleSubjectGroup(ostatniTJ.filter(!_.classHour.combinedClasses),(0 to 7))

  val rests: Seq[TeachersJob] = rest1 ++ rest3 ++ rest5 ++ rest12 ++ rest32 ++ rest52 ++ rest2 ++ rest4 ++ rest22 ++ rest42
  scheduleSubjectGroup(rests,(0 to 7))
  println(rests)

//  def scheduleOneDay(day:Int) {
//    def checkAssgnOk(tj:TeachersJob,day:Int,hour:Int) = {
//      if(!teachersAvailability(day).contains(tj.teacher) ||
//        (tj.classHour.firstSecond && (hour == 0 || hour > 5)) ||
//        (tj.classHour.third && (hour > 5))) false
//      else {
//        val free = tj.classHour.classes.forall(cls => schoolSchedule.schoolSchedule(cls).classSchedule(day)(hour)==null)
//        val nonExcluded = (FIRST_GRADE to LAST_GRADE).forall(gr => {
//          val aux = schoolSchedule.schoolSchedule(gr).classSchedule(day)(hour)
//          aux == null || aux.teacher != tj.teacher
//        })
//        free && nonExcluded
//      }
//    }
//    def isComplete(day:Int, hour:Int) {
//      schoolSchedule.schoolSchedule.forall(cs => cs.classSchedule(day)(hour)!=null)
//    }
//    def assign(tj:TeachersJob,day:Int,hour:Int) {
//      tj.classHour.classes.foreach(gr => {
//        schoolSchedule.schoolSchedule(gr).classSchedule(day)(hour) = tj
//      })
//    }
//    def unassign(tj:TeachersJob,day:Int,hour:Int) {
//      tj.classHour.classes.foreach(gr => {
//        schoolSchedule.schoolSchedule(gr).classSchedule(day)(hour) = null
//      })
//    }
//    def scheduleHour(tjs:ListBuffer[TeachersJob],day:Int,hour:Int) = {
//      if(!isComplete(day,hour)) {
//
//      }
//    }
//  }

  // </schedule>

  printSchedule(schoolSchedule)

  val scheduledJobs: List[TeachersJob] = schoolSchedule.schoolSchedule.foldLeft(List[TeachersJob]())((coll, cs) => coll ++ cs.classSchedule.foldLeft(List[TeachersJob]())((ccoll, ds) => {
    ccoll ++ ds.filter(_ != null)
  }))

  println("Total jobs = "+(teachersJobs.foldLeft(0)((total,tj) => total + tj.classHour.classes.size)+22))
  println("Scheduled jobs = "+scheduledJobs.size)
  println(teachersJobs diff scheduledJobs)

  println("odpol = "+(odpol.valid))
  println("volna = "+(volna.valid))
  println("prvniDruha = "+(prvniDruha.valid))
  println("neprerusene = "+(neprerusene.valid))
  println("exklusivitaUcitele = "+(exklusivitaUcitele.valid))
  println("vvVzdyPoSobe = "+(vvVzdyPoSobe.valid))
  println("dvojHodinoveNePoSobe = "+(dvojHodinoveNePoSobe.valid))
  println("stejnyNeVeStejnyDen = "+(stejnyNeVeStejnyDen.h))
  println("hlavniPredmety = "+(hlavniPredmety.h))
  println("spojenePredmety = "+(spojenePredmety.h))
  println("spravnePrirazene = "+(spravnePrirazene.valid))

  println("reditelUciJenVPoAPa = "+(reditelUciJenVPoAPa.valid))
  println("evaVolnoVUtAPa = "+(evaVolnoVUtAPa.valid))
  println("luckaVolnoVPa = "+(luckaVolnoVPa.valid))
  println("druzinarkaHana = "+(druzinarkaHana.valid))
}

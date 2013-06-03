import scala.collection.immutable.IndexedSeq

object X extends App {

  private val MONDAY = 0
  private val TUESDAY = 1
  private val WEDNESDAY = 2
  private val THURSDAY = 3
  private val FRIDAY = 4

  private val FIRST_HOUR = 0
  private val LAST_HOUR = 7

  private val FIRST_GRADE = 0
  private val LAST_GRADE = 8

  private val VERY_HIGH_H = 10000

  case class ClassHour(val subject:String, val classes:Set[Int]) {
    val arts = subject.contains("Vv")
    val mainSubject = Set("Čj","Aj","M").exists(subject.contains(_))
    val twoHour = Set("Vl", "Př", "D", "Z", "F", "Ch").exists(subject.contains(_))
    val combinedClasses = classes.size>1
    val pe = subject.contains("Tv")
  }

  case class Teacher(val name:String)

  case class TeachersJob(val teacher:Teacher, val classHour:ClassHour)

  class ClassSchedule {
    val classSchedule = {
      val aux = new Array[Array[TeachersJob]](FRIDAY - MONDAY + 1)
      MONDAY to FRIDAY foreach (i => aux(i) = new Array[TeachersJob](LAST_HOUR - FIRST_HOUR + 1))
      aux
    }
  }

  class SchoolSchedule {
    val schoolSchedule = {
      val aux = new Array[ClassSchedule](LAST_GRADE - FIRST_GRADE + 1)
      FIRST_GRADE to LAST_GRADE foreach (i => aux(i) = new ClassSchedule())
      aux
    }
  }

  abstract class Constraint {
    def valid:Boolean
    def preferred:Boolean
    def h:Int
  }

  abstract class NecessaryConstraint extends Constraint {
    def preferred:Boolean = {valid}
    def h:Int = {if(valid) 0 else VERY_HIGH_H}
  }

  abstract class PreferenceConstraint extends Constraint {
    def valid = true
    def preferred = h == 0
  }

  class OdpoledniVUrciteDny(val schoolSchedule:SchoolSchedule, days:Set[Int]) extends NecessaryConstraint {
    def valid = {
      schoolSchedule.schoolSchedule forall (x => x.classSchedule.zipWithIndex forall (x => {
        val daySch = x._1
        !((daySch(LAST_HOUR-1)!=null || daySch(LAST_HOUR)!=null) && !days.contains(x._2))
      }))
    }
  }

  class VolnaHodina(val schoolSchedule:SchoolSchedule) extends NecessaryConstraint {
    def valid = {
      schoolSchedule.schoolSchedule forall(x => x.classSchedule forall (y => (y(5)==null || y(6)==null)))
    }
  }

  class NepreruseneVyucovaniDopoledne(val schoolSchedule:SchoolSchedule) extends NecessaryConstraint {
    def valid = {
      schoolSchedule.schoolSchedule forall (x => x.classSchedule forall (y => 1 to 4 forall (z => y(z)!=null)))
    }
  }

  def morningSchoolNtimes(cs:ClassSchedule,ntimes:Int) = {
    cs.classSchedule.filter(x => x(FIRST_HOUR)!=null).size<=ntimes
  }

  def afternoonSchoolNtimes(cs:ClassSchedule,ntimes:Int) = {
    cs.classSchedule.filter(ds => (ds(LAST_HOUR-1)!=null || ds(LAST_HOUR)!=null)).size<=ntimes
  }

  class PrvniDruha(val schoolSchedule:SchoolSchedule) extends NecessaryConstraint {
    def valid = {
      FIRST_GRADE to (FIRST_GRADE+1) forall (i => morningSchoolNtimes(schoolSchedule.schoolSchedule(i),0) && afternoonSchoolNtimes(schoolSchedule.schoolSchedule(i),0))
    }
  }

  class Treti(val schoolSchedule:SchoolSchedule) extends NecessaryConstraint {
    def valid = {
      morningSchoolNtimes(schoolSchedule.schoolSchedule(FIRST_GRADE+2),1) && afternoonSchoolNtimes(schoolSchedule.schoolSchedule(FIRST_GRADE+2),0)
    }
  }

  class CtvrtaPata(val schoolSchedule:SchoolSchedule) extends NecessaryConstraint {
    def valid = {
      FIRST_GRADE+3 to FIRST_GRADE+4 forall (i => morningSchoolNtimes(schoolSchedule.schoolSchedule(i),1) && afternoonSchoolNtimes(schoolSchedule.schoolSchedule(i),1))
    }
  }

  class JedenUcitelJednaHodina(val schoolSchedule:SchoolSchedule) extends NecessaryConstraint {
    def valid = {
      MONDAY to FRIDAY forall (i => FIRST_HOUR to LAST_HOUR forall (j => {
        val aux = schoolSchedule.schoolSchedule.map(cs => cs.classSchedule(i)(j)).filter(tj=>tj!=null).distinct.map(_.teacher)
        aux.distinct.size == aux.size
      }))
    }
  }

  class PredmetySpravnePrirazeneTridam(val schoolSchedule:SchoolSchedule) extends NecessaryConstraint {
    def valid = {
      schoolSchedule.schoolSchedule.zipWithIndex.forall(csi => csi._1.classSchedule.forall(ds => ds.filter(_!=null).forall(tj => tj.classHour.classes.contains(csi._2))))
    }
  }

  class UcitelUciVUrciteDny(val schoolSchedule:SchoolSchedule, t:Teacher, days:Set[Int]) extends NecessaryConstraint {
    def valid = {
      schoolSchedule.schoolSchedule forall (x => ((MONDAY to FRIDAY).toSet -- days) forall (i => x.classSchedule(i) forall (tj => tj == null || tj.teacher!=t)))
    }
  }

  class UcitelUciVUrciteHodiny(val schoolSchedule:SchoolSchedule, t:Teacher, hours:Set[Int]) extends NecessaryConstraint {
    def valid = {
      schoolSchedule.schoolSchedule.forall(cs => cs.classSchedule forall(ds => (FIRST_HOUR to LAST_HOUR).toSet -- hours forall(i => ds(i)==null || ds(i).teacher!=t)))
    }
  }

  class Druzinar(val schoolSchedule:SchoolSchedule, t:Teacher, normalHour:Int) extends NecessaryConstraint {
    def valid = {
      val findEndOfDay: (Int) => Int = day => schoolSchedule.schoolSchedule.map({
        cs => cs.classSchedule(day).zipWithIndex.foldLeft(0)(
          (a, lastHour) => {
            if (lastHour._1 != null) lastHour._2 else a
          })
      }).foldLeft(0)((b, lh) => if (lh > b) lh else b)

      val endsOfDays: IndexedSeq[Int] = (MONDAY to FRIDAY).map(findEndOfDay)

      endsOfDays.zipWithIndex.forall(endOfDay => {
        schoolSchedule.schoolSchedule.forall(cs => ((normalHour+1) to LAST_HOUR) forall (i => {
          val ds = cs.classSchedule(endOfDay._2)
          ds(i) == null || ds(i).teacher!=t || i==endOfDay._1
        }))
      }
      )
    }
  }

  class VvVzdyPoSobe(val schoolSchedule:SchoolSchedule) extends NecessaryConstraint {
    private val found = 0
    private val not_found = 1
    private val success = 2
    private val failure = 3

    def valid = {schoolSchedule.schoolSchedule.forall(cs => cs.classSchedule.forall(ds => {
      val aux = ds.foldLeft(not_found)((art,tj) => art match {
        case `not_found` => if(tj != null) {
          if(tj.classHour.arts) found else not_found
        } else not_found
        case `found` => if(tj!=null) {
          if(tj.classHour.arts) success
          else if(tj.classHour.pe) found
          else failure
        } else found
        case x:Int => x
      })
      (aux == not_found || aux == success)
    }))
    }

  }

  class DvojhodinnovePredmetyNeVeDnechPoSobe(val schoolSchedule:SchoolSchedule) extends NecessaryConstraint {
    def valid = {
      val twoHourSubjects: IndexedSeq[IndexedSeq[Array[String]]] = (FIRST_GRADE to LAST_GRADE).map(cls => (MONDAY to FRIDAY).map(day => {
        schoolSchedule.schoolSchedule(cls).classSchedule(day).filter(tj => tj!=null && tj.classHour.twoHour).map(tj => tj.classHour.subject)
      }))
      twoHourSubjects.forall(allDaysForClass => {
        allDaysForClass.zipWithIndex.foldLeft((Map[String,Int](),true))((twoHoursSubjectsWithDays,oneDay) => {
          val currentDaysTwoHourSubjects: Array[String] = oneDay._1
          val currentDay: Int = oneDay._2

          val twoInTheSameDay = currentDaysTwoHourSubjects.distinct.size != currentDaysTwoHourSubjects.size

          val ok = currentDaysTwoHourSubjects.forall(subj => {
            if(twoHoursSubjectsWithDays._1.get(subj).exists(day => currentDay-day <= 1)) {
              false
            } else {
              true
            }
          }) && !twoInTheSameDay
          val outMap = currentDaysTwoHourSubjects.foldLeft(twoHoursSubjectsWithDays._1)((m,subj) => m + (subj -> currentDay))

          (outMap,twoHoursSubjectsWithDays._2 && ok)
        })._2
      })
    }
  }

  class StejnyPredmetNeVTenSamyDen(val schoolSchedule:SchoolSchedule) extends PreferenceConstraint {
    def h = {
      schoolSchedule.schoolSchedule.foldLeft(0)((totalPerSchool,cs) => {
        totalPerSchool + cs.classSchedule.foldLeft(0)((totalPerClass,ds) => {
          val subjectsOfTheDay: Array[String] = ds.filter(tj => tj != null && !tj.classHour.arts && !tj.classHour.pe).map(tj => tj.classHour.subject)
          totalPerClass + (subjectsOfTheDay.size - subjectsOfTheDay.distinct.size)
        })
      })
    }
  }

  def penalizeSubjectsInGivenHours(schoolSchedule:SchoolSchedule,selector:(ClassHour => Boolean),hours:Seq[Int]) = {
    schoolSchedule.schoolSchedule.foldLeft(0)((totalPerSchool,cs) => {
      totalPerSchool + cs.classSchedule.foldLeft(0)((totalPerClass,ds) => {
        totalPerClass + hours.map(i => ds(i)).filter(tj => tj!=null && selector(tj.classHour)).size
      })
    })
  }

  class HlavniPredmetyRano(val schoolSchedule:SchoolSchedule) extends PreferenceConstraint {
    def h = penalizeSubjectsInGivenHours(schoolSchedule,(ch => ch.mainSubject),(FIRST_HOUR :: (4 to LAST_HOUR).toList))
  }

  class SpojenePredmetyRano(val schoolSchedule:SchoolSchedule) extends PreferenceConstraint {
    def h = penalizeSubjectsInGivenHours(schoolSchedule,(ch => ch.combinedClasses && ch.mainSubject),5 to LAST_HOUR)
  }

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
  def preassign(tj:TeachersJob,cls:Int,day:Int,hour:Int):Unit = preassign(tj,List(cls),day,hour)
  def preassign(tj:TeachersJob,cls:Iterable[Int],day:Int,hour:Int):Unit = {
    if(!(teachersJobs.contains(tj))) throw new RuntimeException("Preassigning non-existent TJ")
    teachersJobs = teachersJobs diff List(tj)
    cls.foreach(c => schoolSchedule.schoolSchedule(c).classSchedule(day)(hour) = tj)
  }

  preassign(IvaJobs(0),6,FRIDAY,2)
  preassign(IvaJobs(1),6,FRIDAY,3)

  val secondary = Set(5,6,7,8)

  preassign(TeachersJob(Teacher("Lucka"),ClassHour("Tv_Dív 6/7/8/9",secondary)),secondary,MONDAY,5)
  preassign(TeachersJob(Teacher("Lucka"),ClassHour("Tv_Dív 6/7/8/9",secondary)),secondary,THURSDAY,6)
  preassign(TeachersJob(Teacher("Lucka"),ClassHour("Tv_Chl 6/7/8/9",secondary)),secondary,MONDAY,6)
  preassign(TeachersJob(Teacher("Lucka"),ClassHour("Tv_Chl 6/7/8/9",secondary)),secondary,THURSDAY,5)

  // </preassignment>

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

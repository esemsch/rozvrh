import scala.collection.immutable.IndexedSeq

object X extends App {

  class ClassHour(val subject:String, val classes:Set[Int]) {
    val arts = subject=="Vv"
    val mainSubject = Set("Čj","Aj","M").contains(subject)
    val twoHour = Set("Vl", "Př", "D", "Z", "F", "Ch").contains(subject)
    val combinedClasses = classes.size>1
    val pe = subject.contains("Tv")
  }

  class Teacher(val name:String)

  class TeachersJob(val teacher:Teacher, val classHour:ClassHour)

  class ClassSchedule {
    val classSchedule = {
      val aux = new Array[Array[TeachersJob]](5)
      0 to 4 foreach (i => aux(i) = new Array[TeachersJob](8))
      aux
    }
  }

  class SchoolSchedule {
    val schoolSchedule = {
      val aux = new Array[ClassSchedule](9)
      0 to 8 foreach (i => aux(i) = new ClassSchedule())
      aux
    }
  }

  def noSense = throw new RuntimeException("Does not make sense")

  abstract class Constraint {
    def valid:Boolean
    def preferred:Boolean
    def h:Int
  }

  abstract class NecessaryConstraint extends Constraint {
    def preferred:Boolean = {valid}
    def h:Int = {if(valid) 0 else 1000}
  }

  class OdpoledniVUrciteDny(val schoolSchedule:SchoolSchedule, days:Set[Int]) extends NecessaryConstraint {
    def valid = {
      schoolSchedule.schoolSchedule forall (x => x.classSchedule.zipWithIndex forall (x => {
        val daySch = x._1
        !((daySch(6)!=null || daySch(7)!=null) && !days.contains(x._2))
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
    cs.classSchedule.filter(x => x(0)!=null).size<=ntimes
  }

  def afternoonSchoolNtimes(cs:ClassSchedule,ntimes:Int) = {
    cs.classSchedule.filter(ds => (ds(6)!=null || ds(7)!=null)).size<=ntimes
  }

  class PrvniDruha(val schoolSchedule:SchoolSchedule) extends NecessaryConstraint {
    def valid = {
      0 to 1 forall (i => morningSchoolNtimes(schoolSchedule.schoolSchedule(i),0) && afternoonSchoolNtimes(schoolSchedule.schoolSchedule(i),0))
    }
  }

  class Treti(val schoolSchedule:SchoolSchedule) extends NecessaryConstraint {
    def valid = {
      morningSchoolNtimes(schoolSchedule.schoolSchedule(2),1) && afternoonSchoolNtimes(schoolSchedule.schoolSchedule(2),0)
    }
  }

  class CtvrtaPata(val schoolSchedule:SchoolSchedule) extends NecessaryConstraint {
    def valid = {
      3 to 4 forall (i => morningSchoolNtimes(schoolSchedule.schoolSchedule(i),1) && afternoonSchoolNtimes(schoolSchedule.schoolSchedule(i),1))
    }
  }

  class JedenUcitelJednaHodina(val schoolSchedule:SchoolSchedule) extends NecessaryConstraint {
    def valid = {
      0 to 4 forall (i => 0 to 7 forall (j => {
        val aux = schoolSchedule.schoolSchedule.map(cs => cs.classSchedule(i)(j)).filter(tj=>tj!=null).map(_.teacher)
        aux.distinct.size == aux.size
      }))
    }
  }

  class UcitelUciVUrciteDny(val schoolSchedule:SchoolSchedule, t:Teacher, days:Set[Int]) extends NecessaryConstraint {
    def valid = {
      schoolSchedule.schoolSchedule forall (x => ((0 to 4).toSet -- days) forall (i => x.classSchedule(i) forall (tj => tj == null || tj.teacher!=t)))
    }
  }

  class UcitelUciVUrciteHodiny(val schoolSchedule:SchoolSchedule, t:Teacher, hours:Set[Int]) extends NecessaryConstraint {
    def valid = {
      schoolSchedule.schoolSchedule.forall(cs => cs.classSchedule forall(ds => (0 to 7).toSet -- hours forall(i => ds(i)==null || ds(i).teacher!=t)))
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

      val endsOfDays: IndexedSeq[Int] = (0 to 4).map(findEndOfDay)

      endsOfDays.zipWithIndex.forall(endOfDay => {
        schoolSchedule.schoolSchedule.forall(cs => ((normalHour+1) to 7) forall (i => {
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
      val twoHourSubjects: IndexedSeq[IndexedSeq[Array[String]]] = (0 to 8).map(cls => (0 to 4).map(day => {
        schoolSchedule.schoolSchedule(cls).classSchedule(day).filter(tj => tj.classHour.twoHour).map(tj => tj.classHour.subject)
      }))
      twoHourSubjects.forall(allDaysForClass => {
        allDaysForClass.zipWithIndex.foldLeft((Map[String,Int](),true))((twoHoursSubjectsWithDays,oneDay) => {
          val currentDaysTwoHourSubjects: Array[String] = oneDay._1
          val currentDay: Int = oneDay._2

          val ok = currentDaysTwoHourSubjects.forall(subj => {
            if(twoHoursSubjectsWithDays._1.get(subj).forall(day => currentDay-day <= 1)) false else true
          })
          val outMap = currentDaysTwoHourSubjects.foldLeft(twoHoursSubjectsWithDays._1)((m,subj) => m + (subj -> currentDay))

          (outMap,twoHoursSubjectsWithDays._2 && ok)
        })
      })
    }
  }

  val schoolSchedule: SchoolSchedule = new SchoolSchedule()

  val t1 = new Teacher("A")
  val t2 = new Teacher("B")
  val t3 = new Teacher("C")
  val vvt = new Teacher("VvT")

  val ch1 = new ClassHour("P1",Set(1))
  val ch2 = new ClassHour("P2",Set(1))
  val vv = new ClassHour("Vv",Set(1))
  val tv = new ClassHour("Tv chlapci",Set(1))

  val tj11 = new TeachersJob(t1,ch1)
  val tj12 = new TeachersJob(t1,ch2)
  val tj21 = new TeachersJob(t2,ch1)
  val tj22 = new TeachersJob(t2,ch2)
  val tj31 = new TeachersJob(t3,ch1)
  val tj32 = new TeachersJob(t3,ch2)

  val tjvv1 = new TeachersJob(vvt,vv)
  val tjvv2 = new TeachersJob(vvt,vv)
  val tv1 = new TeachersJob(t2,tv)

  schoolSchedule.schoolSchedule(0).classSchedule(0)(1) = tj11
  schoolSchedule.schoolSchedule(0).classSchedule(0)(5) = tj11
  schoolSchedule.schoolSchedule(1).classSchedule(0)(1) = tj22
  schoolSchedule.schoolSchedule(1).classSchedule(0)(5) = tj22
//  schoolSchedule.schoolSchedule(2).classSchedule(0)(6) = tj22
  schoolSchedule.schoolSchedule(2).classSchedule(0)(4) = tj22

  schoolSchedule.schoolSchedule(3).classSchedule(0)(0) = tj32
  schoolSchedule.schoolSchedule(3).classSchedule(0)(1) = tj32
  schoolSchedule.schoolSchedule(3).classSchedule(0)(2) = tj32
  schoolSchedule.schoolSchedule(3).classSchedule(0)(3) = tj32
  schoolSchedule.schoolSchedule(3).classSchedule(0)(4) = tj32
  schoolSchedule.schoolSchedule(3).classSchedule(0)(5) = tj32

  schoolSchedule.schoolSchedule(0).classSchedule(0)(5) = tj32
  schoolSchedule.schoolSchedule(1).classSchedule(0)(5) = tj32
  schoolSchedule.schoolSchedule(2).classSchedule(0)(5) = tj32
  schoolSchedule.schoolSchedule(3).classSchedule(0)(5) = tj32
  schoolSchedule.schoolSchedule(4).classSchedule(0)(5) = tj32
  schoolSchedule.schoolSchedule(5).classSchedule(0)(5) = tj32
  schoolSchedule.schoolSchedule(6).classSchedule(0)(5) = tj32
  schoolSchedule.schoolSchedule(7).classSchedule(0)(5) = tj32
  schoolSchedule.schoolSchedule(8).classSchedule(0)(5) = tj32

  schoolSchedule.schoolSchedule(1).classSchedule(3)(4) = tjvv1
  schoolSchedule.schoolSchedule(1).classSchedule(3)(5) = tv1
  schoolSchedule.schoolSchedule(1).classSchedule(3)(6) = tjvv2

  val odpol = new OdpoledniVUrciteDny(schoolSchedule,Set(0,3))
  val volna = new VolnaHodina(schoolSchedule)
  val prvniDruha = new PrvniDruha(schoolSchedule)
  val neprerusene = new NepreruseneVyucovaniDopoledne(schoolSchedule)
  val t2UciJenVPo = new UcitelUciVUrciteDny(schoolSchedule,t2,Set(0))
  val exklusivitaUcitele = new JedenUcitelJednaHodina(schoolSchedule)
  val t1JenDopoledne = new UcitelUciVUrciteHodiny(schoolSchedule,t1,Set(0,1,2,3,4))
  val t3druzinar = new Druzinar(schoolSchedule,t3,4)
  val vvVzdyPoSobe = new VvVzdyPoSobe(schoolSchedule)

  println(odpol.valid)
  println(volna.valid)
  println(prvniDruha.valid)
  println(neprerusene.valid)
  println(t2UciJenVPo.valid)
  println(exklusivitaUcitele.valid)
  println(t1JenDopoledne.valid)
  println(t3druzinar.valid)
  println(vvVzdyPoSobe.valid)

}

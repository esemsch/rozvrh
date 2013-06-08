package x

case class ClassHour(val subject:String, val classes:Set[Int]) {
  val subjects = """([A-záéěíóúýÁÉÍÓÚÝčďřšťžňČĎŘŠŤŽŇ]+)""".r.findAllIn(subject).matchData.map(_.group(1)).toSet
  val arts = subject.contains("Vv")
  val mainSubject = Set("Čj","Aj","M").exists(subject.contains(_))
  val twoHour = Set("Vl", "Př", "D", "Z", "F", "Ch", "Inf 8", "Inf 9").exists(subject.contains(_))
  val combinedClasses = classes.size>1
  val pe = subject.contains("Tv")
  val firstSecond = classes.contains(FIRST_GRADE) || classes.contains(FIRST_GRADE+1)
  val third = classes.contains(FIRST_GRADE+2)
  val fourthFifth = classes.contains(FIRST_GRADE+3) || classes.contains(FIRST_GRADE+4)
  val secondary = classes.contains(FIRST_GRADE+5) || classes.contains(FIRST_GRADE+6) || classes.contains(FIRST_GRADE+7) || classes.contains(FIRST_GRADE+8)
  val lowestClass = classes.foldLeft(Integer.MAX_VALUE)((min,cls) => if(cls<min) cls else min)
  val free = subject.contains("Volna")
  def isMutuallyExclusive(otherClassHour:ClassHour) = classes.exists(cls => otherClassHour.classes.exists(otherCls => cls==otherCls))
}

case class Teacher(val name:String)

case class TeachersJob(val teacher:Teacher, val classHour:ClassHour) {
  override def toString = classHour.subject + " ("+teacher.name+")"
}

case class Job(val teacher:Teacher, val classHour:ClassHour, val count:Int) {
  def toTeachersJobs = (1 to count).map(i => toTeachersJob).toList
  def toTeachersJob = TeachersJob(teacher,classHour)
  override def toString = toTeachersJob.toString
}

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
    def free(tj:TeachersJob) = tj == null || tj.classHour.pe
    schoolSchedule.schoolSchedule forall(x => x.classSchedule forall (y => (free(y(5)) || free(y(6)))))
  }
}

class NepreruseneVyucovaniDopoledne(val schoolSchedule:SchoolSchedule) extends NecessaryConstraint {
  def valid = {
    schoolSchedule.schoolSchedule forall (x => x.classSchedule forall (y => 1 to 4 forall (z => y(z)!=null)))
  }
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
        if(twoInTheSameDay) {
          println("Violating subjects: "+(currentDaysTwoHourSubjects diff currentDaysTwoHourSubjects.distinct).mkString(","))
        }

        val ok = currentDaysTwoHourSubjects.forall(subj => {
          if(twoHoursSubjectsWithDays._1.get(subj).exists(day => currentDay-day <= 1)) {
            println("Violating subject: subj")
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

class HlavniPredmetyRano(val schoolSchedule:SchoolSchedule) extends PreferenceConstraint {
  def h = penalizeSubjectsInGivenHours(schoolSchedule,(ch => ch.mainSubject),(FIRST_HOUR :: (4 to LAST_HOUR).toList))
}

class SpojenePredmetyRano(val schoolSchedule:SchoolSchedule) extends PreferenceConstraint {
  def h = penalizeSubjectsInGivenHours(schoolSchedule,(ch => ch.combinedClasses && ch.mainSubject),5 to LAST_HOUR)
}

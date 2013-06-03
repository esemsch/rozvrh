object X extends App {

  class ClassHour(val subject:String, val classes:Set[Int])

  class Teacher(val name:String)

  class TeachersJob(val teacher:Teacher, val classHour:ClassHour)

  class ClassSchedule {
    val classSchedule = {
      val aux = new Array[Array[TeachersJob]](5)
      0 to 4 foreach (i => aux(i) = new Array[TeachersJob](8))
      aux
    }

    def daySchedule(day:Int) = classSchedule(day)
  }

  class SchoolSchedule {
    val schoolSchedule = {
      val aux = new Array[ClassSchedule](9)
      0 to 8 foreach (i => aux(i) = new ClassSchedule())
      aux
    }

    def daySchedules(day:Int) = {schoolSchedule.map(_.daySchedule(day))}
    def hourSchedules(day:Int,hour:Int) = {
      schoolSchedule.map(_.daySchedule(day)).map(_(hour))
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

    }
  }

  val schoolSchedule: SchoolSchedule = new SchoolSchedule()

  val t1 = new Teacher("A")
  val t2 = new Teacher("B")

  val ch1 = new ClassHour("P1",Set(1))
  val ch2 = new ClassHour("P2",Set(1))

  val tj11 = new TeachersJob(t1,ch1)
  val tj12 = new TeachersJob(t1,ch2)
  val tj21 = new TeachersJob(t2,ch1)
  val tj22 = new TeachersJob(t2,ch2)

  schoolSchedule.schoolSchedule(0).classSchedule(0)(1) = tj11
  schoolSchedule.schoolSchedule(1).classSchedule(0)(1) = tj22
  schoolSchedule.schoolSchedule(1).classSchedule(0)(5) = tj22
  schoolSchedule.schoolSchedule(2).classSchedule(0)(6) = tj22
  schoolSchedule.schoolSchedule(2).classSchedule(0)(4) = tj22

  val odpol = new OdpoledniVUrciteDny(schoolSchedule,Set(0,3))
  val volna = new VolnaHodina(schoolSchedule)
  val prvniDruha = new PrvniDruha(schoolSchedule)
  val neprerusene = new NepreruseneVyucovaniDopoledne(schoolSchedule)
  val t2UciJenVPo = new UcitelUciVUrciteDny(schoolSchedule,t2,Set(0))
  val exklusivitaUcitele = new JedenUcitelJednaHodina(schoolSchedule)

  println(odpol.valid)
  println(volna.valid)
  println(prvniDruha.valid)
  println(neprerusene.valid)
  println(t2UciJenVPo.valid)
  println(exklusivitaUcitele.valid)

}

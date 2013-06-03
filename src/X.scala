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

  abstract class HourConstraint {
    def valid(cls:Int,day:Int,hour:Int):Boolean
    def preferred(cls:Int,day:Int,hour:Int):Boolean
    def h(cls:Int,day:Int,hour:Int):Int
  }

  abstract class CrossHourConstraint {
    def valid(day:Int,hour:Int):Boolean
    def preferred(day:Int,hour:Int):Boolean
    def h(day:Int,hour:Int):Int
  }

  abstract class DayConstraint {
    def valid(cls:Int,day:Int):Boolean
    def preferred(cls:Int,day:Int):Boolean
    def h(cls:Int,day:Int):Int
  }

  abstract class CrossDayConstraint {
    def valid(day:Int):Boolean
    def preferred(day:Int):Boolean
    def h(day:Int):Int
  }

  abstract class WeekConstraint {
    def valid(cls:Int):Boolean
    def preferred(cls:Int):Boolean
    def h(cls:Int):Int
  }

  abstract class CrossWeekConstraint {
    def valid():Boolean
    def preferred():Boolean
    def h():Int
  }

  class OneTeacherOneHour(val schoolSchedule:SchoolSchedule) extends CrossHourConstraint {
    def valid(day: Int, hour: Int):Boolean = {
      val d = schoolSchedule.hourSchedules(day,hour) filter (x => x!=null) map (x => x.teacher)
      d.distinct.size == d.size
    }
    def preferred(day: Int, hour: Int):Boolean = {valid(day,hour)}
    def h(day: Int, hour: Int):Int = {if(valid(day,hour)) 0 else 1000}
  }

  class SixthAndSeventhInCertainDaysOnly(val schoolSchedule:SchoolSchedule, days:Set[Int]) extends CrossWeekConstraint {
    def valid: Boolean = {
      schoolSchedule.schoolSchedule forall (x => x.classSchedule.zipWithIndex forall (x => {
        val daySch = x._1
        !((daySch(6)!=null || daySch(7)!=null) && !days.contains(x._2))
      }))
    }
    def preferred: Boolean = valid()
    def h(): Int = if(valid()) 0 else 1000
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

  schoolSchedule.schoolSchedule(0).classSchedule(0)(0) = tj11
  schoolSchedule.schoolSchedule(1).classSchedule(0)(0) = tj22
  schoolSchedule.schoolSchedule(1).classSchedule(1)(5) = tj22

  val c = new OneTeacherOneHour(schoolSchedule)
  val odpol = new SixthAndSeventhInCertainDaysOnly(schoolSchedule,Set(0,3))

  println(c.valid(0,0))
  println(odpol.valid)

}

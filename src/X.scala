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
    val schoolSchedule = new Array[ClassSchedule](9)

    def daySchedules(day:Int) = {schoolSchedule.map(_.daySchedule(day))}
    def hourSchedules(day:Int,hour:Int) = {
      schoolSchedule.map(_.daySchedule(day)).map(_(hour))
    }
  }

  abstract class HourConstraint(schoolSchedule:SchoolSchedule) {
    def valid(cls:Int,day:Int,hour:Int)
    def preferred(cls:Int,day:Int,hour:Int)
    def h(cls:Int,day:Int,hour:Int)
  }

  abstract class CrossHourConstraint(schoolSchedule:SchoolSchedule) {
    def valid(day:Int,hour:Int)
    def preferred(day:Int,hour:Int)
    def h(day:Int,hour:Int)
  }

  abstract class DayConstraint(schoolSchedule:SchoolSchedule) {
    def valid(cls:Int,day:Int)
    def preferred(cls:Int,day:Int)
    def h(cls:Int,day:Int)
  }

  abstract class CrossDayConstraint(schoolSchedule:SchoolSchedule) {
    def valid(day:Int)
    def preferred(day:Int)
    def h(day:Int)
  }

  abstract class WeekConstraint(schoolSchedule:SchoolSchedule) {
    def valid(cls:Int)
    def preferred(cls:Int)
    def h(cls:Int)
  }

  abstract class CrossWeekConstraint(schoolSchedule:SchoolSchedule) {
    def valid()
    def preferred()
    def h()
  }

}

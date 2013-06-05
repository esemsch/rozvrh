package object x {

  def DAY_NAME(d:Int) = {
    d match {
      case `MONDAY` => "Pondělí"
      case `TUESDAY` => "Úterý"
      case `WEDNESDAY` => "Středa"
      case `THURSDAY` => "Čtvrtek"
      case `FRIDAY` => "Pátek"
    }
  }
  val MONDAY = 0
  val TUESDAY = 1
  val WEDNESDAY = 2
  val THURSDAY = 3
  val FRIDAY = 4

  val FIRST_HOUR = 0
  val LAST_HOUR = 7

  val FIRST_GRADE = 0
  val LAST_GRADE = 8

  val VERY_HIGH_H = 10000


  def morningSchoolNtimes(cs:ClassSchedule,ntimes:Int) = {
    cs.classSchedule.filter(x => x(FIRST_HOUR)!=null).size<=ntimes
  }

  def afternoonSchoolNtimes(cs:ClassSchedule,ntimes:Int) = {
    cs.classSchedule.filter(ds => (ds(LAST_HOUR-1)!=null || ds(LAST_HOUR)!=null)).size<=ntimes
  }

  def penalizeSubjectsInGivenHours(schoolSchedule:SchoolSchedule,selector:(ClassHour => Boolean),hours:Seq[Int]) = {
    schoolSchedule.schoolSchedule.foldLeft(0)((totalPerSchool,cs) => {
      totalPerSchool + cs.classSchedule.foldLeft(0)((totalPerClass,ds) => {
        totalPerClass + hours.map(i => ds(i)).filter(tj => tj!=null && selector(tj.classHour)).size
      })
    })
  }
  
}

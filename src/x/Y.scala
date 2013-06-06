package x

object Y extends App {

  var jobs = Data.data2

  val schoolSchedule = new SchoolSchedule

  // <preassignment>
  def preassign(job:Job,day:Int,hour:Int):Unit = {
    jobs = jobs diff List(job)
    job.classHour.classes.foreach(c => schoolSchedule.schoolSchedule(c).classSchedule(day)(hour) = job.toTeachersJob)
  }

  preassign(Job(Teacher("Iva"),ClassHour("Rj 7",Set(6)),2),FRIDAY,2)
  preassign(Job(Teacher("Iva"),ClassHour("Rj 7",Set(6)),2),FRIDAY,3)

  val secondary = Set(5,6,7,8)

  preassign(Job(Teacher("Lucka"),ClassHour("Tv_Dív 6/7/8/9",secondary),2),MONDAY,5)
  preassign(Job(Teacher("Lucka"),ClassHour("Tv_Dív 6/7/8/9",secondary),2),THURSDAY,6)
  preassign(Job(Teacher("Lucka"),ClassHour("Tv_Chl 6/7/8/9",secondary),2),MONDAY,6)
  preassign(Job(Teacher("Lucka"),ClassHour("Tv_Chl 6/7/8/9",secondary),2),THURSDAY,5)

  preassign(Job(Teacher("Tereza"),ClassHour("Vv 6/7",Set(5,6)),2),MONDAY,4)
  preassign(Job(Teacher("Tereza"),ClassHour("Vv 6/7",Set(5,6)),2),MONDAY,7)
  preassign(Job(Teacher("Tereza"),ClassHour("Vv 8/9",Set(7,8)),2),THURSDAY,7)
  // </preassignment>

  case class Possibility(day:Int,hour:Int)

  trait Group {
    def getM:Int
    def update(j:Job,p:Possibility,noRevert:Boolean)
    def unUpdate
    def isScheduled:Boolean
    def isImpossible:Boolean
  }

  trait Availability {
    def getPossibilities:Set[Possibility]

    var plainPossibilities:Set[Possibility] = null

    val hoursByGrades = Map(1 -> (1 to 5), 2 -> (1 to 5), 3 -> (0 to 5), 4 -> (0 to 7),
      5 -> (0 to 7), 6 -> (0 to 7), 7 -> (0 to 7), 8 -> (0 to 7), 9 -> (0 to 7))

    var updated:Possibility = null

    def update(thisJob:Job,scheduledJob:Job,p:Possibility,noRevert:Boolean) {
      val exclusive = thisJob.classHour.isMutuallyExclusive(scheduledJob.classHour) || thisJob.teacher==scheduledJob.teacher
      if (exclusive) {
        val aux = plainPossibilities - p
        if (aux.size < plainPossibilities.size) {
          plainPossibilities = aux
          if(!noRevert) updated = p
        }
      }
      if (noRevert) {
        updated = null
      }
    }

    def unUpdate {
      if (updated!=null) {
        plainPossibilities = plainPossibilities + updated
        updated = null
      }
    }

    protected def checkSchedulable(job:Job,p:Possibility) = {
      val hourSch = schoolSchedule.schoolSchedule.map(cs => cs.classSchedule(p.day)(p.hour))
      val free = job.classHour.classes.forall(cls => hourSch(cls)==null)
      val teacherOk = hourSch.forall(tj => tj==null || tj.teacher!=job.teacher)
      free && teacherOk
    }

    protected def getPlainPossibilities(job:Job,prefHours:Seq[Int]) = {
      cnt = cnt + 1
      if(cnt%100000==0)println(cnt)
      if (plainPossibilities==null) {
        plainPossibilities = (MONDAY to FRIDAY).flatMap(d => prefHours.map(h => Possibility(d,h)).filter(p => checkSchedulable(job,p))).toSet
      }
      plainPossibilities
    }
  }

  var cnt = 0

  class MainSubjectAvailability(job:Job,schoolSchedule:SchoolSchedule) extends Availability {
    override def getPossibilities = getPlainPossibilities(job,1 to 4)
  }

  class RestOfSubjectsAvailability(job:Job,schoolSchedule:SchoolSchedule) extends Availability {
    override def getPossibilities = getPlainPossibilities(job,hoursByGrades(job.classHour.lowestClass))
  }

  class TwoHourSubjectAvailability(job:Job,schoolSchedule:SchoolSchedule) extends Availability {
    private def isConsequent(tj1:TeachersJob,tj2:TeachersJob) = {
      ((tj1.classHour.classes diff tj2.classHour.classes).size == 0 || (tj2.classHour.classes diff tj1.classHour.classes).size == 0) &&
        ((tj1.classHour.subjects diff tj2.classHour.subjects).size < tj1.classHour.subjects.size)
    }
    override def getPossibilities = {
      val tj = job.toTeachersJob
      (MONDAY to FRIDAY).find(day => (FIRST_GRADE to LAST_GRADE).exists(cls => (FIRST_HOUR to LAST_HOUR).exists(h => {
        val otherTj = schoolSchedule.schoolSchedule(cls).classSchedule(day)(h)
        otherTj!=null && (isConsequent(tj,otherTj) || isConsequent(otherTj,tj))
      }))) match {
        case None => {
          getPlainPossibilities(job,hoursByGrades(tj.classHour.lowestClass))
        }
        case Some(day) => {
          getPlainPossibilities(job,hoursByGrades(tj.classHour.lowestClass)).filter(p => math.abs(p.day-day)>1)
        }
      }
    }
  }

  def getAvailability(job:Job,schoolSchedule:SchoolSchedule):Availability = {
    if(job==null || schoolSchedule==null) null
    else if(job.classHour.mainSubject) new MainSubjectAvailability(job,schoolSchedule)
    else if(job.classHour.twoHour) new TwoHourSubjectAvailability(job,schoolSchedule)
    else new RestOfSubjectsAvailability(job,schoolSchedule)
  }

  class SimpleGroup(val job:Job, val schoolSchedule:SchoolSchedule) extends Group {
    private val availability = getAvailability(job,schoolSchedule)
    private var left = if(job!=null) job.count else 0
    def getLeft = left
    private var posCache:Set[Possibility] = if(job!=null) availability.getPossibilities else null

    def getM = posCache.size-left
    def update(j:Job,p:Possibility,noRevert:Boolean) {
      availability.update(job,j,p,noRevert)
      posCache = availability.getPossibilities
    }
    def unUpdate {availability.unUpdate}
    def isScheduled = left == 0
    def isImpossible = left>0 && posCache.size==0
    def getPossibilities:Set[Possibility] = posCache

    def schedule(p:Possibility) {
      left = left-1
      job.classHour.classes.foreach(cls => {
        schoolSchedule.schoolSchedule(cls).classSchedule(p.day)(p.hour) = job.toTeachersJob
      })
    }
    def unschedule(p:Possibility) {
      left = left+1
      job.classHour.classes.foreach(cls => {
        schoolSchedule.schoolSchedule(cls).classSchedule(p.day)(p.hour) = null
      })
    }

    override def toString = job.toTeachersJob.toString

  }

  val simpleGroups = jobs.map(j => new SimpleGroup(j,schoolSchedule))
  val groups = simpleGroups

  def getM(j:Job,p:Possibility) = {
    groups.filter(!_.isScheduled).foldLeft(Integer.MAX_VALUE)((min,sg)=>{
      sg.update(j,p,false)
      math.min(min,sg.getM)
    })
//    groups.filter(!_.isScheduled).foldLeft(0)((total,sg)=>{
//      sg.update(j,p,false)
//      (total + sg.getM)
//    })
  }

  def getBestPossibility(sg:SimpleGroup) = {
    val pos = sg.getPossibilities
    pos.foldLeft((Possibility(0,0),Integer.MIN_VALUE))((best,pos) => {
      sg.schedule(pos)
      val m = getM(sg.job,pos)
      sg.unschedule(pos)
      groups.foreach(_.unUpdate)
      if(m>best._2) (pos,m)
      else best
    })
  }

  def getBestGroupAndPossibility = {
    simpleGroups.filter(!_.isScheduled).foldLeft((new SimpleGroup(null,null),Possibility(0,0),Integer.MIN_VALUE))((best,sg) => {
      val curr = getBestPossibility(sg)
      if(curr._2>best._3) (sg,curr._1,curr._2)
      else best
    })
  }

  var total = jobs.foldLeft(0)((total,j) => total + j.count)
  while(simpleGroups.exists(!_.isScheduled) && simpleGroups.forall(!_.isImpossible)) {
    val best = getBestGroupAndPossibility
    total = total - 1
    println(total+": "+best)
    best._1.schedule(best._2)
    groups.foreach(_.update(best._1.job,best._2,true))
  }

  Output.printSchedule(schoolSchedule)

  Checker.check(schoolSchedule,jobs.flatMap(_.toTeachersJobs))

  if(simpleGroups.exists(_.isImpossible)) {
    println("!!!SCHEDULING FAILED!!!")
    println("Reason: "+simpleGroups.filter(_.isImpossible).map(sg => sg.job.toString+" left = "+sg.getLeft))
  }
}

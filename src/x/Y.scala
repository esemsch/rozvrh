package x

object Y extends App {

  val jobs = Data.data2

  case class Possibility(day:Int,hour:Int)

  trait Group {
    def getM:Int
    def update(j:Job,p:Possibility)
    def isScheduled:Boolean
    def isImpossible:Boolean
  }

  trait Availability {
    def getPossibilities:Seq[Possibility]
  }

  var cnt = 0

  class MainSubjectAvailability(job:Job,schoolSchedule:SchoolSchedule) extends Availability {
    private def checkSchedulable(p:Possibility) = {
      val hourSch = schoolSchedule.schoolSchedule.map(cs => cs.classSchedule(p.day)(p.hour))
      val free = job.classHour.classes.forall(cls => hourSch(cls)==null)
      val teacherOk = hourSch.forall(tj => tj==null || tj.teacher!=job.teacher)
      free && teacherOk
    }
    override def getPossibilities = {
      cnt = cnt + 1
      if(cnt%100000==0)println(cnt)
      (MONDAY to FRIDAY).flatMap(d => (1 to 4).map(h => Possibility(d,h)).filter(p => checkSchedulable(p)))
    }
  }

  def getAvailability(job:Job,schoolSchedule:SchoolSchedule):Availability = {
    if(job==null || schoolSchedule==null) null
    else if(job.classHour.mainSubject) new MainSubjectAvailability(job,schoolSchedule)
    else null
  }

  class SimpleGroup(val job:Job, val schoolSchedule:SchoolSchedule) extends Group {
    private val availability = getAvailability(job,schoolSchedule)
    private var left = if(job!=null) job.count else 0
    private var posCache:Seq[Possibility] = if(job!=null) availability.getPossibilities else null

    def getM = posCache.size-left
    def update(j:Job,p:Possibility) {posCache = availability.getPossibilities}
    def isScheduled = left == 0
    def isImpossible = left>0 && posCache.size==0
    def getPossibilities:Seq[Possibility] = posCache

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

  val schoolSchedule = new SchoolSchedule

  val simpleGroups = jobs.filter(j => j.classHour.mainSubject).map(j => new SimpleGroup(j,schoolSchedule))
  val groups = simpleGroups

  def getM(j:Job,p:Possibility) = {
    groups.foldLeft(0)((total,sg)=>{
      sg.update(j,p)
      (total + sg.getM)
    })
  }

  def getBestPossibility(sg:SimpleGroup) = {
    val pos = sg.getPossibilities
    pos.foldLeft((Possibility(0,0),Integer.MIN_VALUE))((best,pos) => {
      sg.schedule(pos)
      val m = getM(sg.job,pos)
      sg.unschedule(pos)
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

  while(simpleGroups.exists(!_.isScheduled) && simpleGroups.forall(!_.isImpossible)) {
    val best = getBestGroupAndPossibility
    println(best)
    best._1.schedule(best._2)
    groups.foreach(_.update(best._1.job,best._2))
  }

  Output.printSchedule(schoolSchedule)
}

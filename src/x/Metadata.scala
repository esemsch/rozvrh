package x

object Metadata extends App {

  def setBit(int:Int,index:Int) = {
    int | math.pow(2,index).toInt
  }
  val teachers = Data.data._2.toList
  val tiles = Data.data3._1.filter(j => true).map(j => {
    val clss = j.classHour.classes.foldLeft(0)((result,cls) => setBit(result,cls-1))
    val teacher = setBit(0,teachers.indexOf(j.teacher))
    Tile(clss,teacher,-1,j)
  }).zipWithIndex.map(ti => Tile(ti._1.classes,ti._1.teacher,ti._2,ti._1.job))

  val rows = H.tileIndexRows.map(r => r.map(ti => tiles(ti)).toArray).toArray

  val stats = rows.map(r => (r.map(t => t.job).mkString(" || "),r.size,r.foldLeft(Integer.MAX_VALUE)((min,t) => if(min>t.job.count) t.job.count else min)))
  val format: ((String, Int, Int)) => String = x => x._1 + " --- TT = " + x._2 + " TA = " + x._3
//  println(stats.sortBy(x => x._2).map(format).mkString("\n"))
//  println(stats.sortBy(x => x._3).map(format).mkString("\n"))

  rows.map(r => {
    val minInstances: Int = r.foldLeft(Integer.MAX_VALUE)((min, t) => {
      val instances = rows.filter(x => x != r && x.exists(t2 => t2 == t)).size
      if (instances < min) instances else min
    })
    (r,minInstances)
  }).toList.sortBy(x => x._2).foreach(x => println(x._1.map(o => o.job).mkString(" || ")+" --- "+x._2))
}

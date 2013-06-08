package x

case class Tile(classes:Int,teacher:Int,id:Int,job:Job)

object Tiles extends App {
  val places = (MONDAY to FRIDAY).map(d => (FIRST_HOUR to LAST_HOUR).map(h => Array(0,0)).toArray).toArray

  def applicable(t:Tile,d:Int,h:Int) = {
    ((places(d)(h)(0) & t.classes) | (places(d)(h)(1) & t.teacher)) == 0
  }

  def applyTile(t:Tile,d:Int,h:Int) {
    places(d)(h)(0) = places(d)(h)(0) | t.classes
    places(d)(h)(1) = places(d)(h)(1) | t.teacher
  }

  def revertTile(t:Tile,d:Int,h:Int) {
    places(d)(h)(0) = places(d)(h)(0) ^ t.classes
    places(d)(h)(1) = places(d)(h)(1) ^ t.teacher
  }

  val teachers = Data.data._2.toList
  val tiles = Data.data2.zipWithIndex.map(ji => {
    val j = ji._1
    val id = ji._2
    def setBit(int:Int,index:Int) = {
      int | math.pow(2,index).toInt
    }
    val clss = j.classHour.classes.foldLeft(0)((result,cls) => setBit(result,cls-1))
    val teacher = setBit(0,teachers.indexOf(j.teacher))
    Tile(clss,teacher,id,j)
  })

  val placed = tiles.map(t => Array(-1,-1))

  tiles.foreach(t => {
    val isPlaced = (MONDAY to FRIDAY).exists(d => (FIRST_HOUR to LAST_HOUR).exists(h => {
      if(applicable(t,d,h)) {
        applyTile(t,d,h)
        placed(t.id)(0) = d
        placed(t.id)(1) = h
        true
      } else false
    }))
    if(!isPlaced) {
      println("Cannot place")
    }
  })

  Output.printTiles(places,tiles,placed)

}

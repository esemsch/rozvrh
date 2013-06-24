package solver

import java.io._

object H extends App {

  def tileIndexRows = {
    val rows =  scala.io.Source.fromFile("rows.txt").getLines().toArray
    rows.map(line => line.split("\\s*\\|\\s*").map(_.toInt).toArray).toArray
  }

  def calcRows = {
    val (tilesSolver,tiles,places,counts,placed,mapToPlaced,tilesPerDay,jobs,tilesLookup,teachers) = TilesSolver.factory

    val rows = tilesSolver.calcRows(MONDAY,1,Set(0,1,2,3,4,5,6,7,8))

    val writer = new PrintWriter(new File("rows.txt"))
    writer.write(rows.map(r => r.mkString(" | ")).mkString("\n"))
    writer.flush
    writer.close
  }

  calcRows
}



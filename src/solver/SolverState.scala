package solver

import scala.collection.mutable

case class SolverState(
  tilesSolver:TilesSolver,
  tiles:Array[Tile],
  places:Array[Array[Array[Int]]],
  counts:Array[Int],
  placed:Array[Array[Int]],
  mapToPlaced:Array[Int],
  tilesPerDay:Array[(Array[Int],Tile)],
  jobs: (scala.List[Job], mutable.HashMap[Job, scala.List[TeachersJob]]),
  tilesLookup: Map[String, Map[(Set[Int], Boolean), Tile]],
  teachers: List[Teacher]
)

object SolverState {

  def copy(solverState:SolverState) = {
      val newPlaces = solverState.places.map(a => a.map(b => b.map(c => c)))
      val newCounts = solverState.counts.map(a => a)
      val newPlaced = solverState.placed.map(a => a.map(b => b))
      val newMapToPlaced = solverState.mapToPlaced.map(a => a)
      val newTilesPerDay = solverState.tilesPerDay.map(a => (a._1.map(b => b),a._2))

      val tilesSolver = new TilesSolver(solverState.tiles,newPlaces,newCounts,newPlaced,newMapToPlaced,newTilesPerDay)


      SolverState(
        tilesSolver,
        solverState.tiles,
        newPlaces,
        newCounts,
        newPlaced,
        newMapToPlaced,
        newTilesPerDay,
        solverState.jobs,
        solverState.tilesLookup,
        solverState.teachers
      )
    }

}

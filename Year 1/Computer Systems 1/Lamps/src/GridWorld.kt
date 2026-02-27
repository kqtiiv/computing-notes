import javax.swing.text.Position

enum class Terrain {
    WATER,
    FOREST,
    SWAMP,
    ROCKS,
}

// enum class WorldKind {
//    BOUNDED,
//    DEADLY,
//    RANDOM,
// }

// defining own exception using inheritance
class DeadPlayerException(
    message: String,
) : Exception(message)

abstract class GridWorld(
    private val width: Int,
    private val height: Int,
) {
    private val grid: Array<Array<Terrain>> = randomTerrain()
    private var position: Pair<Int, Int> = randomPosition()

    fun up() = updatePosition(position.copy(second = position.second - 1))

    fun down() = updatePosition(position.copy(second = position.second + 1))

    fun left() = updatePosition(position.copy(first = position.first - 1))

    fun right() = updatePosition(position.copy(first = position.first + 1))

    private fun updatePosition(newPosition: Pair<Int, Int>) {
        if (newPosition.first in 0..<width && newPosition.second in 0..<height) {
            position = newPosition
            return
        }
//        when (worldKind) {
//            WorldKind.BOUNDED -> Unit
//            WorldKind.RANDOM -> position = randomPosition()
//            WorldKind.DEADLY -> throw DeadPlayerException("Fell off world!")
//        }

        position = handleOverrun(newPosition)
    }

    protected open fun handleOverrun(newPosition: Pair<Int, Int>): Pair<Int, Int> =
        throw NotImplementedError("Should be provided as subclasses")

    fun randomTerrain(): Array<Array<Terrain>> {}

    fun randomPosition(): Pair<Int, Int> {}
}

class DeadlyWorld(
    width: Int,
    height: Int,
) : GridWorld(width, height) {
    override fun handleOverrun(newPosition: Pair<Int, Int>): Nothing = throw DeadPlayerException("Fell off world!")
}

class BoundedGridWorld(
    width: Int,
    height: Int,
) : GridWorld(width, height) {
    override fun handleOverrun(newPosition: Pair<Int, Int>): Pair<Int, Int> =
        Pair(
            first = max(0, min(newPosition.first, width - 1)),
            second = max(0, min(newPosition.second, height - 1)),
        )
}

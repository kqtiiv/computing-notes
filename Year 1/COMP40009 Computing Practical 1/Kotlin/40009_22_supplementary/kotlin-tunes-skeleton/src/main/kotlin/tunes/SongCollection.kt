package tunes

class SongCollection {
    private class Song(
        val name: String,
        val tune: Tune,
    )

    private class TreeNode(
        var song: Song,
        var left: TreeNode? = null,
        var right: TreeNode? = null,
    )

    private var root: TreeNode? = null

    fun addSong(
        name: String,
        tune: Tune,
    ) {
        val song = Song(name, tune)

        fun addSongHelper(node: TreeNode?): TreeNode {
            if (node != null) {
                val nodeName = node.song.name
                when {
                    name == nodeName -> throw UnsupportedOperationException()
                    name < nodeName -> node.left = addSongHelper(node.left)
                    else -> node.right = addSongHelper(node.right)
                }
            } else {
                return TreeNode(song)
            }
            return node
        }
        root = addSongHelper(root)
    }

    fun getTune(name: String): Tune {
        fun getTuneHelper(node: TreeNode?): Tune =
            if (node != null) {
                val nodeName = node.song.name
                when {
                    name == nodeName -> node.song.tune
                    name < nodeName -> getTuneHelper(node.left)
                    else -> getTuneHelper(node.right)
                }
            } else {
                throw NoSuchElementException()
            }
        return getTuneHelper(root)
    }

    fun getSongNames(): List<String> {
        val returnValue = mutableListOf<String>()

        fun traversal(node: TreeNode?) {
            if (node != null) {
                traversal(node.left)
                returnValue.add(node.song.name)
                traversal(node.right)
            } else {
                return
            }
        }

        traversal(root)
        return returnValue
    }
}

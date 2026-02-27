private const val MAX_BRIGHTNESS: Int = 10
private const val MIN_BRIGHTNESS: Int = 1

open class Lamp(
    isOn: Boolean,
) {
    protected var isOn = isOn
        private set

    open fun pressSwitch() {
        isOn != isOn
    }

    override fun toString(): String =
        if (isOn) {
            "LIGHT"
        } else {
            "(darkness)"
        }
}

class DimmingLamp(
    isOn: Boolean,
) : Lamp(isOn) { // dimminglamp extends lamp
    private var brightness: Int =
        if (isOn) {
            MAX_BRIGHTNESS
        } else {
            0
        }

    override fun pressSwitch() {
        super.pressSwitch()
        if (isOn) {
            brightness = MAX_BRIGHTNESS
        } else {
            brightness = 0
        }
    }

    fun up(): DimmingLamp {
        if (isOn && brightness < MAX_BRIGHTNESS) {
            brightness++
        }
        return this
    }

    fun down(): DimmingLamp {
        if (isOn && brightness > MIN_BRIGHTNESS) {
            brightness--
        }
        return this
    }

    override fun toString(): String =
        super.toString() +
            if (isOn) {
                ": " + "*".repeat(brightness)
            } else {
                ""
            }
}

fun main() {
    val myDimmingLamp = DimmingLamp(false)

    println(myDimmingLamp)
}

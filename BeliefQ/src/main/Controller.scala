// Just your usual (?) state machine stuff

package beliefq

import spinal.core._
import spinal.lib._

class Controller extends Component {
    val state = out port State()
    state := State.idle
}
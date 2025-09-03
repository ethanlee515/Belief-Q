package beliefq

import spinal.core._
import spinal.lib._

case class VToCInputs(params: BeliefQParams, deg: Int) extends Bundle {
  import params._
  val messages = Vec.fill(deg)(message_t())
  val prior = message_t()
}

class VToC(params: BeliefQParams, deg: Int) extends Component {
  import params._
  /* -- IO -- */
  val inputs = in port Flow(VToCInputs(params, deg))
  val output = out port Flow(Vec.fill(deg)(message_t()))
  val delays = 2 * deg + 1
  /* logic */
  // Forward + backward sweep trick
  object LocalState extends SpinalEnum {
    val idle, forward, backward, done = newElement()
  }
  val state = RegInit(LocalState.idle)
  val ctr = Reg(UInt(log2Up(deg) bits)) init(0)
  val accumulator = Reg(message_t)
  // Potentially unnecessary
  val messages = Vec.fill(deg)(Reg(message_t))
  val result = Vec.fill(deg)(Reg(message_t))
  output.valid := (state === LocalState.done)
  output.payload := result
  switch(state) {
    is(LocalState.idle) {
      when(inputs.valid) {
        state := LocalState.forward
        accumulator := inputs.prior
        ctr := 0
        for(i <- 0 until deg) {
          messages(i) := inputs.messages(i)
        }
      }
    }
    is(LocalState.forward) {
      result(ctr) := accumulator
      when(ctr =/= deg - 1) {
        if(deg > 1) {
          ctr := ctr + 1
        }
        accumulator := (accumulator + messages(ctr)).truncated
      } otherwise {
        state := LocalState.backward
        accumulator := BigDecimal(0)
      }
    }
    is(LocalState.backward) {
      result(ctr) := (result(ctr) + accumulator).truncated
      when(ctr =/= 0) {
        if(deg > 1) {
          ctr := ctr - 1
        }
        accumulator := (accumulator + messages(ctr)).truncated
      } otherwise {
        state := LocalState.done
      }
    }
    is(LocalState.done) {
      state := LocalState.idle
    }
  }
}


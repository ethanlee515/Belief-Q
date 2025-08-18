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
  val inputs = slave Stream(VToCInputs(params, deg))
  val output = out port Flow(Vec.fill(deg)(message_t()))
  /* logic */
  // Forward + backward sweep trick
  object State extends SpinalEnum {
    val idle, forward, backward, done = newElement()
  }
  val state = RegInit(State.idle)
  val ctr = Reg(UInt(log2Up(deg) bits)) init(0)
  val accumulator = Reg(message_t)
  val messages = Vec.fill(deg)(Reg(message_t))
  val result = Vec.fill(deg)(Reg(message_t))
  output.valid := (state === State.done)
  output.payload := result
  inputs.ready := (state === State.idle)
  switch(state) {
    is(State.idle) {
      when(inputs.valid) {
        state := State.forward
        accumulator := inputs.prior
        ctr := 0
        for(i <- 0 until deg) {
          messages(i) := inputs.messages(i)
          // result(i) := BigDecimal(0)
        }
      }
    }
    is(State.forward) {
      result(ctr) := accumulator
      when(ctr =/= deg - 1) {
        ctr := ctr + 1
        accumulator := (accumulator + messages(ctr)).truncated
      } otherwise {
        state := State.backward
        accumulator := BigDecimal(0)
      }
    }
    is(State.backward) {
      result(ctr) := (result(ctr) + accumulator).truncated
      when(ctr =/= 0) {
        ctr := ctr - 1
        accumulator := (accumulator + messages(ctr)).truncated
      } otherwise {
        state := State.done
      }
    }
    is(State.done) {
      state := State.idle
    }
  }
}


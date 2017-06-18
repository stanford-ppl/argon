package argon.lang.direct

import forge._

trait AssertApi {
  /** Static methods **/
  @api def assert(cond: MBoolean, msg: MString): MUnit = MUnit(AssertOps.assert(cond.s, Some(msg.s)))
  @api def assert(cond: MBoolean): MUnit = MUnit(AssertOps.assert(cond.s, None))
}

package argon.lang.direct

import forge._

trait AssertApi {
  /** Static methods **/

  /** Checks that the given condition `cond` is true at runtime.
    * If not, exits the program with the given message.
    */
  @api def assert(cond: MBoolean, msg: MString): MUnit = MUnit(AssertOps.assert(cond.s, Some(msg.s)))

  /** Checks that the given condition `cond` is true at runtime.
    * If not, exits the program with a generic exception.
    */
  @api def assert(cond: MBoolean): MUnit = MUnit(AssertOps.assert(cond.s, None))
}

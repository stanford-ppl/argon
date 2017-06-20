package argon.lang.cake

import argon.core.cake.ArgonCake
import argon.lang.direct._

/** Static functions, implicit conversions, app-facing type aliases **/
trait ArgonLangExternal extends ArgonApi with ArgonCake with ArgonExternalAliases
trait ArgonLangInternal extends ArgonExp with ArgonInternalAliases

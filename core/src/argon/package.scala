import argon.core.cake.{Cast, Lift}

package object argon extends ArgonInternal {

  type Lift[A,B] = Lift[A,B]
  type Cast[A,B] = Cast[A,B]
  //type ? = MetaAny[_]


}

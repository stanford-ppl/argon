package object argon extends ArgonInternal {
  type SrcCtx = org.virtualized.SourceContext
  type Lift[A,B] = argon.core.Lift[A,B]
  type Cast[A,B] = argon.core.Cast[A,B]
  //type ? = MetaAny[_]

  def ctx(implicit context: SrcCtx): SrcCtx = context
  def state(implicit state: State): State = state
}

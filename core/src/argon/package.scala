package object argon extends ArgonCore {
  type SrcCtx = org.virtualized.SourceContext

  def ctx(implicit context: SrcCtx): SrcCtx = context
  def state(implicit state: State): State = state
}

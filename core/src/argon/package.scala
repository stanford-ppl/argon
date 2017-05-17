package object argon extends ArgonCore with nodes.Nodes {
  type SrcCtx = org.virtualized.SourceContext

  def ctx(implicit context: SrcCtx): SrcCtx = context
  def state(implicit state: State): State = state
}

package argon.core

import argon.core.cake._

abstract class ArgonCore
  extends Reporting
  with BlocksLayer
  with DefsLayer
  with EffectsLayer
  with Lattices
  with MetadataLayer
  with Scheduling
  with StagedTypes
  with Staging
  with Statements
  with SymbolsLayer
{
  type SrcCtx = org.virtualized.SourceContext
  def ctx(implicit context: SrcCtx): SrcCtx = context
  def state(implicit state: State): State = state
}

private[argon] object ops extends ArgonCore
package argon.core.cake

import argon.core.Config

trait ArgonCake
  extends LayerReporting
    with LayerBlocks
    with LayerDefs
    with LayerEffects
    with LayerLattices
    with LayerMetadata
    with LayerScheduling
    with LayerStagedTypes
    with LayerStaging
    with LayerStatements
    with LayerSymbols
{
  type SrcCtx = org.virtualized.SourceContext
  def ctx(implicit context: SrcCtx): SrcCtx = context
  def state(implicit state: State): State = state
  def config(implicit state: State): Config = state.config

  // Based on performance testing, LongMap is slightly faster than others (even with casting)
  type OrderCache = scala.collection.mutable.LongMap[(NodeId,Int)] // EdgeId -> (NodeId,Int)
  def OrderCache() = new scala.collection.mutable.LongMap[(NodeId,Int)]()
}

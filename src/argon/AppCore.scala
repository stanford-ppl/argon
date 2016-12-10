package argon

import argon.ops.VoidApi

trait AppCore extends VoidApi {
  def main(): Void
}

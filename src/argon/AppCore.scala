package argon

import argon.ops.VoidAPI

trait AppCore extends VoidAPI {
  def main(): Void
}

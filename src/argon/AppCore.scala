package argon

import argon.ops.ArrayApi

trait AppCore extends ArrayApi {
  var args: MArray[Text]
  var stagingArgs: scala.Array[java.lang.String]
  def main(): scala.Unit
}

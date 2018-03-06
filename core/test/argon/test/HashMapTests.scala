package argon.test

import org.scalatest.{FlatSpec, Matchers}
import virtualized._

object GroupByReduceTest extends Test {
  import argon.test.api._

  @virtualize
  def main() {
    val array = Array.tabulate(50){i => random[Int](50) }

    val map = array.groupByReduce{x => x % 10.to[Int] }{x => 1.to[Int]}{_+_}
    val max = map.values.reduce{(a,b) => if (a > b) a else b}

    array.foreach{x => print("" + x + " ") }
    println("")
    println("max: " + max)

    // Super awesome console histogram
    // Hack: need range syntax in argon
    Array.tabulate(max){i => i}.foreach{m =>
      map.keys.foreach { k =>
        if (map(k) >= max - m) print("X  ") else print("   ")
      }
      println("")
    }
    map.keys.foreach{k => print("" + k + "  ") }
    println("")
  }
}




object Kmeans extends Test {
  import argon.test.api._
  testArgs = List("1", "8")

  @virtualize
  def main() {
    val iters = args(0).to[Int]
    val N = args(1).to[Int]
    val K = 4   //args(2).to[SInt];
    val D = 16  //args(3).to[SInt];

    val Is = Array.tabulate(iters){i => i}
    val Ks = Array.tabulate(K){i => i}
    val Ds = Array.tabulate(D){i => i}
    val Ns = Array.tabulate(N){i => i}

    val pts = Ns.map{i => Ds.map{d => if (d == D-1) 1.to[Int] else random[Int](10) + i }}
    val cnts = Ks.map{i => Ds.map{d => if (d == D-1) 1.to[Int] else random[Int](10) + (i*N/K) }}

    val cts = Array.empty[Array[Int]](K)
    Ks.foreach { k =>
      cts(k) = Ds.map{i => cnts(k).apply(i) }
    }

    Is.foreach { i =>
      def dist[T:Type:Num](p1: Array[T], p2: Array[T]) = p1.zip(p2){(a,b) => (a - b)*(a - b) }.reduce(_+_)

      // Make weighted points
      val map = pts.groupByReduce{pt =>
        val dists = cts.map{ct => dist(ct, pt) }
        dists.zip(Ks){(a,b) => pack(a,b) }
             .reduce{(a,b) => if (a._1 < b._1) a else b}
             ._2  // minIndex
      }{pt => pt}{(x,y) => x.zip(y){_+_} }

      // Average
      Ks.foreach { k =>
        if (map.contains(k)) {
          val wp = map(k)
          val n  = wp(D - 1)
          cts(k) = Ds.map{d => if (d == D-1) 1.to[Int] else wp(d)/n }
        }
        else {
          cts(k) = Ds.map{d => 0.to[Int] }
        }
      }
    }

    println("\n\nOriginal Centers:")
    Ks.foreach{i => Ds.foreach{j => print("" + cnts(i).apply(j) + ", ") }; println() }
    println("\n\nOriginal Points:")
    Ns.foreach{i => Ds.foreach{j => print("" + pts(i).apply(j) + ", ") }; println() }
    println("\n\nCenters:")
    Ks.foreach{i => Ds.foreach{j => print("" + cts(i).apply(j) + ", ") }; println() }
  }
}

object CodeMotionTest extends Test {
  import argon.test.api._
  testArgs = List("8")

  @virtualize
  def main() {
    val N = args(0).to[Int]
    val Ns = Array.tabulate(N){i => i}
    val Ks = Array.tabulate(10){i => i}

    val pts = Ns.map{n => random[Int](10) }
    val cts = Array.empty[Array[Int]](10)

    // Histogram
    val map: HashMap[Int,Int] = pts.groupByReduce{pt => pt }{pt => 1.to[Int] }{_+_}

    Ks.foreach { k =>
      if (map.contains(k)) {
        val wp = map(k)
        cts(k) = Ns.map{n => wp + n }
      }
      else {
        cts(k) = Ns.map{d => 0.to[Int] }
      }
    }

    println("\n\nPts:")
    Ns.foreach{i => print("" + pts(i) + ", ") }; println()
    println("\n\nCts:")
    Ks.foreach{i => Ns.foreach{j => print("" + cts(i).apply(j) + ", ") }; println() }
  }
}

class HashMapTests extends FlatSpec with Matchers {
  "GroupByReduceTest" should "compile and run" in { GroupByReduceTest.main(Array.empty) }

  "Kmeans" should "compile and run" in { Kmeans.main(Array.empty) }

  "CodeMotionTest" should "compile and run" in { CodeMotionTest.main(Array.empty) }
}

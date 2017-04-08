package argon.util
import argon.core.Reporting

class ArgParser(val scriptName: String) extends Reporting {
  val MAX_COLS = 80

  val description = ""
  var arguments = Map[String, Argument]()
  def addArg(arg: Argument) = arguments += arg.pattern -> arg

  trait Argument {
    def pattern: String
    def help: String
    def group: String
    def action(args: List[String]): List[String]
    def shorthand: String
  }

  def usage(): Unit = println(s"Use $scriptName -h for help")
  def help(): Unit = {
    println(scriptName)
    println(description)
    println("")
    val maxKeyLength: Int = arguments.values.map(_.shorthand.length).max + 1
    val descCols = MAX_COLS - maxKeyLength

    val groups = arguments.values.toList.groupBy(_.group)

    groups.foreach { case (name, opts) =>
      if (name != "") println(name)

      opts.sortBy(_.pattern.dropWhile(_ == '-')).foreach { arg =>
        arg.help.split("\n").zipWithIndex.foreach {case (line,i) =>
          var desc = line
          val spacing = " " * (maxKeyLength - arg.shorthand.length)
          if (i == 0) {
            val takeLength = {
              if (desc.length > descCols) {
                val i = desc.zipWithIndex.lastIndexWhere { case (c,i) => c == ' ' && i < descCols }
                if (i < 0) descCols else i
              }
              else descCols
            }
            println(arg.shorthand + spacing + desc.take(takeLength))
            desc = desc.drop(descCols)
          }
          while (desc.length > 0) {
            println(" " * maxKeyLength + desc.take(descCols))
            desc = desc.drop(descCols)
          }
        }
      }
      println("")
    }

    sys.exit()
  }

  class Switch(arg: Char, val help: String, val group: String = "", act: => Unit) extends Argument {
    val pattern = "-" + arg
    def action(args: List[String]) = { act; args }
    def shorthand = s"[$pattern]"
  }
  def Switch(arg: Char, help: String, group: String = "")(act: => Unit) = new Switch(arg,help,group,act)


  class LongSwitch(arg: String, val help: String, val group: String = "", act: => Unit) extends Argument {
    val pattern = "--" + arg
    def action(args: List[String]) = { act; args }
    def shorthand = s"[$pattern]"
  }
  def LongSwitch(arg: String, help: String, group: String = "")(act: => Unit) = new LongSwitch(arg,help,group,act)


  class Named(arg: String, val help: String, val group: String = "", act: String => Unit) extends Argument {
    val pattern = "--" + arg
    def action(args: List[String]) = {
      try {
        if (args.head.startsWith("-")) throw new Exception("Illegal switch argument")
        act(args.head); args.tail
      }
      catch {case _: Throwable =>
        error(s"Argument $pattern expected one argument")
        logError()
        args
      }
    }
    def shorthand = s"[$pattern ${arg.toUpperCase}]"
  }
  def Named(arg: String, help: String, group: String = "")(act: String => Unit) = new Named(arg,help,group,act)

  def parseArgs(args: List[String], unmatched: List[String]): (Boolean, List[String]) = {
    if (!args.isEmpty) {
      arguments.get(args.head) match {
        case Some(opt) => parseArgs(opt.action(args.tail), unmatched)
        case None => parseArgs(args.tail, unmatched :+ args.head)
      }
    }
    else {
      if (hadErrors) usage()
      unmatched.foreach{arg =>
        if (arg.startsWith("-")) warn("Unrecognized switch " + arg)
      }
      (hadErrors,unmatched)
    }
  }

  addArg(Switch('h', "show this help message and exit"){ help() })
  addArg(LongSwitch("help", "show this help message and exit"){ help() })
}

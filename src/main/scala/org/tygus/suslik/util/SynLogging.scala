package org.tygus.suslik.util

import org.tygus.suslik.language.Statements.Procedure
import org.tygus.suslik.logic.FunSpec
import org.tygus.suslik.logic.Specifications._
import org.tygus.suslik.logic.smt.SMTSolving
import org.tygus.suslik.synthesis.{SynConfig, SynthesisRule}
import scalaz.DList

import scala.collection.mutable

/**
  * @author Ilya Sergey
  */

sealed abstract class SynLogging {
  def print(s: String): Unit

  def println(s: String): Unit

  def println(): Unit

  def printlnErr(s: String): Unit

  def testPrintln(s: String, color: String = Console.BLACK): Unit
}

/**
  * Different loggind levels
  */
object SynLogLevels {

  object Verbose extends SynLogging {
    override def printlnErr(s: String): Unit = System.err.println(s)

    override def print(s: String): Unit = Console.print(s)

    override def println(s: String): Unit = Console.println(s)

    override def testPrintln(s: String, color: String = Console.BLACK): Unit = {
      Console.println(s"$color$s${Console.BLACK}")
    }

    override def println(): Unit = Console.println()
  }

  object Test extends SynLogging {
    // Mute standard print
    override def println(s: String): Unit = {}

    override def print(s: String): Unit = {}

    override def println(): Unit = {}

    override def printlnErr(s: String): Unit = Console.println(s)

    override def testPrintln(s: String, color: String = Console.BLACK): Unit = {
      Console.println(s"$color$s${Console.BLACK}")
    }
  }

}

class SynStats {
  private var backtracking: Int = 0
  private var successful: Int = 0
  private var lasting: Int = 0
  private var saved_results_positive: Int = 0
  private var saved_results_negative: Int = 0
  private var recalled_results_positive: Int = 0
  private var recalled_results_negative: Int = 0

  // doesn't have to be a tree... can just be a list
  //private var inference_tree : mutable.TreeMap[Int, String] = mutable.TreeMap.empty[Int, String]
  private var inference_tree : List[String] = List.empty[String]

  def bumpUpBacktracing() {
    backtracking = backtracking + 1
  }

  def bumpUpSuccessfulRuleApp() {
    successful = successful + 1
  }

  def bumpUpLastingSuccess() {
    lasting = lasting + 1
  }

  def bumpUpSavedResultsNegative() {
    saved_results_negative += 1
  }
  def bumpUpRecalledResultsNegative() {
    recalled_results_negative += 1
  }

  def bumpUpSavedResultsPositive() {
    saved_results_positive +=  1
  }
  def bumpUpRecalledResultsPositive() {
    recalled_results_positive +=  1
  }

  def addRuleToInferenceTree(message: String): Unit = {
    //inference_tree += (key -> message)
    inference_tree = message :: inference_tree
  }

  def removeRuleFromInferenceTree(): Unit = {
    //inference_tree -= key
    inference_tree = inference_tree.drop(1)
  }

  def createStringInferenceTree() : String = {
  //todo need to add indentation
    //for ((k,v) <- inference_tree) Console.println("key: %s, value: %s\n", k, v)

    inference_tree.foldRight("")((acc, entry) => {
      //acc + entry._2
      acc + entry
    })

    //"heyyy"
  }

  def numBack: Int = backtracking
  def numSucc : Int = successful
  def numLasting : Int = lasting
  def numSavedResultsPositive : Int = saved_results_positive
  def numRecalledResultsPositive : Int = recalled_results_positive
  def numSavedResultsNegative : Int = saved_results_negative
  def numRecalledResultsNegative : Int = recalled_results_negative
  def smtCacheSize: Int = SMTSolving.cacheSize
  var total_goals_saved = 0

}

abstract sealed class SynCertificate
case class SynAxiom(goal: Goal, rule: SynthesisRule) extends SynCertificate
case class SynTree(subgoals: Seq[SynCertificate]) extends SynCertificate

// TODO: refactor me to make more customizable
object SynStatUtil {

  import java.io.{File, FileWriter}

  val myStats = "stats.csv"
  val myFile = new File(myStats)
  val initRow: String =
    List("Name", "Time", "Spec Size", "Code Size", "Backtrackings", "Lasting", "Total", "SMT Cache").mkString(", ") + "\n"

  def init(config: SynConfig){
    if (config.logToFile) {
      if (myFile.exists()) myFile.delete()
      myFile.createNewFile()
      using(new FileWriter(myFile, true))(_.write(initRow))
    }
  }

  def using[A <: {def close() : Unit}, B](resource: A)(f: A => B): B =
      try f(resource) finally resource.close()

  def log(name: String, time: Long, config: SynConfig, spec: FunSpec, stats: Option[(Procedure, SynStats)]): Unit = {
    if (config.logToFile) {
      val statRow = (stats match {
        case Some((proc, st)) => List(proc.body.size, st.numBack, st.numLasting, st.numSucc, st.smtCacheSize)
        case None => DList.replicate(4, "FAIL").toList
      }).mkString(", ")

      val specSize = spec.pre.size + spec.post.size
      val data = s"$name, $time, $specSize, $statRow, ${config.pp}\n"
      using(new FileWriter(myFile, true))(_.write(data))
    }
  }

}
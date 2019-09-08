package org.tygus.suslik.logic.unification

import org.tygus.suslik.language.Expressions.Var
import org.tygus.suslik.language.Substitution
import org.tygus.suslik.logic.{SFormula, _}
import org.tygus.suslik.logic.Specifications._

/**
  * @author Ilya Sergey
  */
object SpatialUnification extends UnificationBase {

  type UAtom = Heaplet

  val precise: Boolean = true


  def tryUnify(target: UAtom, source: UAtom, nonFreeInSource: Set[Var]): Seq[Substitution] =
    tryUnify(target, source, nonFreeInSource, tagsMatter = false)

  /**
    * Tries to unify two heaplets `target` and `source`, assuming `source` has
    * variables that are either free or in `nonFreeInSource`.
    *
    * If successful, returns a substitution from `source`'tFrame fresh variables to `target`'tFrame variables
    */

  // TODO [Immutability] use regular var to MTag substitution
  def tryUnify(target: UAtom, source: UAtom,
               nonFreeInSource: Set[Var],
               // Take the application level tags into the account
               // should be ignored when used from the *-intro rule
               tagsMatter: Boolean = true): Seq[Substitution] = {
    import MTag._
    assert(target.vars.forall(nonFreeInSource.contains), s"Not all variables of ${target.pp} are in $nonFreeInSource")

    (target, source) match {
      case (PointsTo(x@Var(_), o1, y, m1), PointsTo(a@Var(_), o2, b, m2)) =>
        if (o1 != o2 ||
          (!sub(m1, m2) && !MTag.substitutable(m1, m2))
        ) Nil else {
          assert(nonFreeInSource.contains(x))
          assert(y.vars.forall(nonFreeInSource.contains))
          val sbst = for {
            d1 <- genSubst(x, a, nonFreeInSource)
            _v2 = b.subst(d1)
            d2 <- genSubst(y, _v2, nonFreeInSource)
          } yield {
            assertNoConflict(d1, d2)
            d1 ++ d2
          }
          // if... make substitution for tag here

          if (m1 != m2) {
            (sbst ++ genSubstMut(m1, m2, nonFreeInSource)).toList
          } else {
            sbst.toList
          }
        }
      case (Block(x1@Var(_), s1, m1), Block(x2@Var(_), s2, m2)) =>
        if (s1 != s2 || (!pre(m1, m2) && !MTag.substitutable(m1, m2))) Nil else {
          assert(nonFreeInSource.contains(x1))
          (genSubst(x1, x2, nonFreeInSource) ++ genSubstMut(m1, m2, nonFreeInSource)).toList
        }
      case (SApp(p1, es1, targetTag, m1, sm1), SApp(p2, es2, sourceTag, m2, sm2)) =>
        // Only unify predicates with variables as arguments
        // if es2.forall(_.isInstanceOf[Var])

        if (p1 != p2 || es1.size != es2.size ||
          (!pre(m1, m2) && !MTag.checkLists(sm1, sm2)) ||
          (targetTag != sourceTag && tagsMatter)) Nil

        else {
          val pairs = es1.zip(es2)

          // Collect the mapping from the predicate parameters
          (pairs.foldLeft(Some(Substitution()): Option[Substitution]) {
            case (opt, (x1, x2)) => opt match {
              case None => None
              case Some(acc) =>
                genSubst(x1, x2, nonFreeInSource) match {
                  case Some(sbst) =>
                    assertNoConflict(acc, sbst)
                    Some(acc ++ sbst)
                  case None => None
                }
            }
          }
          ++
            ((sm1, sm2) match {
              case (Some(mut1), Some(mut2)) =>
                val mutZip : List[(MTag, MTag)] = mut1.zip(mut2)
                if (mut1.forall{case a => !MTag.isMutable(a)} || mut2.forall{case a => !MTag.isMutable(a)}) None
                mutZip.foldLeft(Some(Substitution()): Option[Substitution]) {
                  case (opt, (hmut, hmut2)) => opt match {
                    case None => None
                    case Some(acc) =>
                      genSubstMut(hmut, hmut2, nonFreeInSource) match {
                        case Some(sbst) =>
                          assertNoConflict(acc, sbst)
                          Some(acc ++ sbst)
                        case None => None
                      }
                  }
                }
              case (_, _) => None})).toList
        }
      case _ => Nil
    }
  }


  /**
    * Check that two lists of heaplets have a chance to be unified
    */
  protected def checkShapesMatch(cs1: List[UAtom], cs2: List[UAtom]): Boolean = {
    val (ps1, ps2) = (cs1.filter(_.isInstanceOf[PointsTo]), cs2.filter(_.isInstanceOf[PointsTo]))
    val (bs1, bs2) = (cs1.filter(_.isInstanceOf[Block]), cs2.filter(_.isInstanceOf[Block]))
    val (as1, as2) = (cs1.filter(_.isInstanceOf[SApp]), cs2.filter(_.isInstanceOf[SApp]))

    // Check sizes
    if (ps1.size != ps2.size) return false
    if (bs1.size != bs2.size) return false
    if (as1.size != as2.size) return false
    import MTag._

    // Check matching blocks
    val checkMatchingBlocks = (bs1: List[Heaplet], bs2: List[Heaplet]) =>
      bs1.forall {
        case Block(_, s1, m1) => bs2.exists { case Block(_, s2, m2) => s1 == s2 && 
          pre(m1, m2); case _ => false }
        case _ => false
      }

    if (!checkMatchingBlocks(bs1, bs2) || !checkMatchingBlocks(bs2, bs1)) return false

    // Check matching blocks
    val checkMatchingApps = (as1: List[Heaplet], as2: List[Heaplet]) =>
      as1.forall {
        case SApp(x1, xs1, _, m1, sm1) =>
          as2.exists { case SApp(x2, xs2, _, m2, sm2) => x1 == x2 && xs1.size == xs2.size &&
            pre(m1, m2); case _ => false }
        case _ => false
      }
    if (!checkMatchingApps(as1, as2) || !checkMatchingApps(as2, as1)) return false

    true
  }


  protected def extractChunks(goal: UnificationGoal): List[UAtom] = goal.formula.sigma.chunks

}

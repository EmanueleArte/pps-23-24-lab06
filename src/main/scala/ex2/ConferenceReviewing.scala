package ex2

import ex2.Question.FINAL

import scala.collection.*

enum Question:
  case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

trait ConferenceReviewing:
  def loadReview(art: Int, scores: Map[Question, Int]): Unit

  def loadReview(art: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  def orderedScores(art: Int, q: Question): Seq[Int]

  def averageFinalScore(art: Int): Double

  def acceptedArticles: Set[Int]

  def sortedAcceptedArticles: Seq[(Int, Double)]

  /**
   * @return a map from articles to their average "weighted final score", namely,
   *         the average value of CONFIDENCE*FINAL/10
   *         Note: this method is optional in this exam
   */
  def averageWeightedFinalScoreMap: Map[Int, Double]


class ConferenceReviewingImpl extends ConferenceReviewing:
  private var reviews = immutable.Seq.empty[(Int, Map[Question, Int])]

  def loadReview(art: Int, scores: Map[Question, Int]): Unit = scores match
    case _ if scores.size == Question.values.length => reviews = reviews.appended((art, scores))

  def loadReview(art: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    loadReview(art, Map(
      Question.RELEVANCE -> relevance,
      Question.SIGNIFICANCE -> significance,
      Question.CONFIDENCE -> confidence,
      Question.FINAL -> fin)
    )

  def orderedScores(art: Int, q: Question): Seq[Int] =
    reviews.filter(_._1 == art).map(_._2(q)).sortWith(_ < _)

  private def calcScore(init: Double)(scores: Map[Question, Int], q: Question*)(op: (Double, Int) => Double)(finalOp: Double => Double): Double =
    var res = init
    for i <- q do
      res = op(res, scores(i))
    finalOp(res)

  def averageFinalScore(art: Int): Double = reviews.foldLeft(0.0)((acc, review) => review match
    case (a, scores) if a == art => acc + calcScore(0.0)(scores, Question.FINAL)(_ + _)(_ / reviews.count(_._1 == art))
    case _ => acc)

  def acceptedArticles: Set[Int] =
    reviews.filter(_._2(Question.RELEVANCE) >= 8).filter(a => averageFinalScore(a._1) > 5).map(_._1).toSet

  def sortedAcceptedArticles: Seq[(Int, Double)] =
    acceptedArticles.map(a => (a, averageFinalScore(a))).toSeq.sortBy(_._2)

  private def calcWeightedScore(art: Int, scores: Map[Question, Int]): Double =
    calcScore(1.0)(scores, Question.CONFIDENCE, Question.FINAL)(_ * _)(_ / 10 / reviews.count(_._1 == art))

  def averageWeightedFinalScoreMap: Map[Int, Double] = reviews.foldLeft(Map.empty[Int, Double])((acc, review) => review match
    case (a, scores) => acc + acc.get(a).map(x => x + calcWeightedScore(a, scores)).map(a -> _).getOrElse(a -> calcWeightedScore(a, scores))
  )

object ConferenceReviewing:
  def apply(): ConferenceReviewing = new ConferenceReviewingImpl

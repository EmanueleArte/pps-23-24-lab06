package ex2

import org.junit.Assert.assertEquals
import org.junit.{Before, Test}

class ReviewingTest:

  private var cr: ConferenceReviewing = _

  @Before
  def init(): Unit =
    cr = ConferenceReviewing()
    // carico una revisione per l'articolo 1:
    // - 8 per relevance, significance e final
    // - 7 per confidence
    // si ricordi che l'ordine delle domande è: relevance, significance, confidence, final
    cr.loadReview(1, 8, 8, 6, 8) // 4.8 è il voto finale pesato (usato da averageWeightedFinalScoreMap)

    // e simile per gli altri
    cr.loadReview(1, 9, 9, 6, 9) // 5.4
    cr.loadReview(2, 9, 9, 10, 9) // 9.0
    cr.loadReview(2, 4, 6, 10, 6) // 6.0
    cr.loadReview(3, 3, 3, 3, 3) // 0.9
    cr.loadReview(3, 4, 4, 4, 4) // 1.6
    cr.loadReview(4, 6, 6, 6, 6) // 3.6
    cr.loadReview(4, 7, 7, 8, 7) // 5.6

    var map: Map[Question, Int] = Map()
    map = map + ((Question.RELEVANCE, 8))
    map = map + ((Question.SIGNIFICANCE, 8))
    map = map + ((Question.CONFIDENCE, 7))
    map = map + ((Question.FINAL, 8))

    cr.loadReview(4, map)
    cr.loadReview(5, 6, 6, 6, 10) // 6.0
    cr.loadReview(5, 7, 7, 7, 10) // 7.0


  @Test def testOrderedScores(): Unit = {
    // l'articolo 2 ha preso su RELEVANCE i due voti 4,9
    assertEquals(cr.orderedScores(2, Question.RELEVANCE), Seq(4, 9))
    // e simile per gli altri
    assertEquals(cr.orderedScores(4, Question.CONFIDENCE), Seq(6, 7, 8))
    assertEquals(cr.orderedScores(5, Question.FINAL), Seq(10, 10))
  }

  @Test def testAverageFinalScore(): Unit = {
    // l'articolo 1 ha preso voto medio su FINAL pari a 8.5, con scarto massimo 0.01
    assertEquals(cr.averageFinalScore(1), 8.5, 0.01)
    // e simile per gli altri
    assertEquals(cr.averageFinalScore(2), 7.5, 0.01)
    assertEquals(cr.averageFinalScore(3), 3.5, 0.01)
    assertEquals(cr.averageFinalScore(4), 7.0, 0.01)
    assertEquals(cr.averageFinalScore(5), 10.0, 0.01)
  }

  @Test def testAcceptedArticles(): Unit = {
    // solo gli articoli 1,2,4 vanno accettati, avendo media finale >=5 e almeno un voto su RELEVANCE >= 8
    assertEquals(cr.acceptedArticles, Set(1, 2, 4))
  }

  @Test def testSortedAcceptedArticles(): Unit = {
    // articoli accettati, e loro voto finale medio
    assertEquals(cr.sortedAcceptedArticles, Seq((4, 7.0), (2, 7.5), (1, 8.5)))
  }

  @Test def optionalTestAverageWeightedFinalScore(): Unit = {
    // l'articolo 1 ha media pesata finale pari a (4.8+5.4)/2 = 5,1, con scarto massimo 0.01
    assertEquals(cr.averageWeightedFinalScoreMap(1), (4.8 + 5.4) / 2, 0.01)
    // e simile per gli altri
    assertEquals(cr.averageWeightedFinalScoreMap(2), (9.0 + 6.0) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap(3), (0.9 + 1.6) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap(4), (3.6 + 5.6 + 5.6) / 3, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap(5), (6.0 + 7.0) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap.size, 5)
  }

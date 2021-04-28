package fpinscala.parallelism

import java.util.concurrent.Executors

import fpinscala.parallelism.Par._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  val s = Executors.newFixedThreadPool(20)

  "choice" should "run the true case when the condition is true" in {
    val output = choice(unit(true))(lazyUnit("true"), lazyUnit(throw new RuntimeException("boom")))
    Par.run(s)(output) should be ("true")
  }

  it should "run the false case when the condition is false" in {
    val output = choice(unit(false))(lazyUnit(throw new RuntimeException("boom")), lazyUnit("false"))
    Par.run(s)(output) should be ("false")
  }

  "choiceN" should "pick the 2nd value based on the first" in {
    val expected = (0 until 10).map(_.toString).toList
    val choices: List[Par[String]] = expected.map(unit)

    val output: Par[List[String]] = sequence((0 until 10).toList.map(i => choiceN(unit(i))(choices)))

    Par.run(s)(output) should equal(expected)
  }

  "choiceMap" should "pick the 2nd value based on the key" in {
    val expected = (0 until 10).map(i => i -> i.toString)
    val choicesMap: Map[Int, Par[String]] = expected.toMap.view.mapValues(unit).toMap

    val output: Par[List[(Int, String)]] =
      sequence((0 until 10).toList.map(i => map(choiceMap(unit(i))(choicesMap))(x => i -> x)))

    Par.run(s)(output) should equal(expected)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()

    s.shutdown()
  }
}

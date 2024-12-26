package jurisk.optimization

import org.scalatest.freespec.AnyFreeSpec

import scala.util.Random

class GeneticAlgorithmSpec extends AnyFreeSpec {
  "GeneticAlgorithm" - {
    "should find the maximum of a simple function" in {
      val result = GeneticAlgorithm.geneticAlgorithm(
        populationSize = 100,
        fitnessFunction = (x: Double) => -1 * x * x + 2 * x + 3,
        crossover = (a: Double, b: Double) => {
          val mid = (a + b) / 2
          (mid, mid)
        },
        mutate = (x: Double) => x + Random.nextDouble() - 0.5,
        randomChromosome = () => Random.nextDouble() * 10,
        generations = 1000,
      )

      val Eps = 0.01
      assert(result > 1 - Eps)
      assert(result < 1 + Eps)
    }
  }
}

package jurisk.optimization

import scala.util.Random

object GeneticAlgorithm {
  private val Debug = false
  private type Population[T] = Vector[T]

  def geneticAlgorithm[Chromosome](
    populationSize: Int,
    fitnessFunction: Chromosome => Double,
    crossover: (Chromosome, Chromosome) => (Chromosome, Chromosome),
    mutate: Chromosome => Chromosome,
    randomChromosome: () => Chromosome,
    generations: Int,
  ): Chromosome = {
    def evolve(population: Population[Chromosome]): Population[Chromosome] = {
      val fitnessScores = population map fitnessFunction
      val bestIndex     = fitnessScores.indexOf(fitnessScores.max)
      val best          = population(bestIndex)
      if (Debug) {
        println(s"Best: $best with fitness ${fitnessScores(bestIndex)}")
      }

      Vector
        .fill(populationSize / 2) {
          val parent1          = selection(population, fitnessScores)
          val parent2          = selection(population, fitnessScores)
          val (child1, child2) = crossover(parent1, parent2)
          Vector(mutate(child1), mutate(child2))
        }
        .flatten
    }

    def selection(
      population: Population[Chromosome],
      fitnessScores: Vector[Double],
    ): Chromosome = {
      val minFitness        = fitnessScores.min
      val normalizedScores  = fitnessScores map (_ - minFitness)
      val totalFitness      = normalizedScores.sum
      val selectedValue     = Random.nextDouble() * totalFitness
      val cumulativeFitness = normalizedScores.scanLeft(0.0)(_ + _).tail
      val selectedIndex     = cumulativeFitness.indexWhere(_ >= selectedValue)
      population(selectedIndex)
    }

    val initialPopulation = Vector.fill(populationSize)(randomChromosome())
    val finalPopulation   = (1 to generations).foldLeft(initialPopulation) {
      (pop, idx) =>
        if (Debug) {
          println(s"Generation $idx")
        }
        evolve(pop)
    }

    finalPopulation.maxBy(fitnessFunction)
  }
}

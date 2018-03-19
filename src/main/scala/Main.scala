import common._

object Main {

	val TO_MILLISECONDS = 1000000f
	var finished = false

	def main(args: Array[String]): Unit = {
		val iterations = 1000

		val sequentialTime = computeAvgTime(sequentialBogosort, iterations)
		val parallelTime = computeAvgTime(auxParallelBogosort, iterations)

		println(s"Sequential implementation : $sequentialTime ms")
		println(s"\nParallel implementation : $parallelTime ms")
		println(s"\nOverrall speedup : " + sequentialTime / parallelTime)
	}

	def auxParallelBogosort(in: Seq[Int]): Seq[Int] = {
		finished = false
		val output = parallel(parallelBogosort(in), parallelBogosort(in), parallelBogosort(in), parallelBogosort(in))
		if (isSorted(output._1)) output._1
		else if (isSorted(output._2)) output._2
		else if (isSorted(output._3)) output._3
		else output._4
	}

	def parallelBogosort(in: Seq[Int]): Seq[Int] = {
		var input = in
		var i = 1
		while (!finished && !isSorted(input)) {
			input = shuffle(input)
			i += 1
		}
		finished = true
		input
	}

	def sequentialBogosort(in: Seq[Int]): Seq[Int] = {
		var input = in
		var i = 1
		while (!isSorted(input)) {
			input = shuffle(input)
			i += 1
		}
		input
	}

	def isSorted(array: Seq[Int]): Boolean = array.sorted == array

	def shuffle(array: Seq[Int]): Seq[Int] = util.Random.shuffle(array)

	def computeAvgTime(sort: Seq[Int] => Seq[Int], iterations: Int): Double = {
		val input = Array(1, 4, 8, 5, 6, 2)
		var totalTime: Double = 0
		(1 to iterations).foreach(_ => {
			val in_time = System.nanoTime()
			sort(input)
			val out_time = System.nanoTime()
			totalTime += out_time - in_time
		})
		val avg = totalTime / (iterations * TO_MILLISECONDS)
		avg
	}
}

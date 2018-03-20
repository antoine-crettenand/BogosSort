import common._

object Main {

	val TO_MILLISECONDS = 1000000f
	val DEFAULT_SIZE = 5
	val DEFAULT_NUMBER_OF_ITERATIONS = 1000

	def main(args: Array[String]): Unit = {

		val sequentialTime = computeAvgTime(sequentialBogosort)
		val parallelTime = computeAvgTime(parallelBogosort)

		println("Average Computing time :")
		println(s"Sequential implementation : $sequentialTime ms")
		println(s"\nParallel implementation : $parallelTime ms")
		println(s"\nOverrall speedup : " + sequentialTime / parallelTime)
	}

	/**
	  * Multi-Threaded BogoSort -> Calls 4 threads to simultaneously shuffle an array until it is sorted
	  * If one thread achieves sorting, every other thread stops as well
	  *
	  * @param in array to sort
	  * @return sorted array
	  */
	def parallelBogosort(in: Seq[Int]): Seq[Int] = {
		var stopNow = false

		def auxParallelBogosort(in: Seq[Int]): Seq[Int] = {
			var input = in
			var i = 1
			while (!stopNow && !isSorted(input)) {
				input = shuffle(input)
				i += 1
			}
			stopNow = true
			input
		}

		stopNow = false
		val output = parallel(auxParallelBogosort(in), auxParallelBogosort(in), auxParallelBogosort(in),
			auxParallelBogosort(in))
		if (isSorted(output._1)) output._1
		else if (isSorted(output._2)) output._2
		else if (isSorted(output._3)) output._3
		else output._4
	}

	/**
	  * Randomly shuffles the array until it is sorted
	  *
	  * @param in the array to be sorted
	  * @return sorted array
	  */
	def sequentialBogosort(in: Seq[Int]): Seq[Int] = {
		var input = in
		var i = 1
		while (!isSorted(input)) {
			input = shuffle(input)
			i += 1
		}
		input
	}

	/**
	  * Checks if array is sorted
	  *
	  * @param array to check
	  * @return true if so, false otherwise
	  */
	def isSorted(array: Seq[Int]): Boolean = array.sorted == array

	/**
	  * Shuffle the elements of an array
	  *
	  * @param array, not null
	  * @return a shuffled array
	  */
	def shuffle[A](array: Seq[A]): Seq[A] = util.Random.shuffle(array)

	/**
	  * Compute the average running time of an algorithm over iterations steps.
	  *
	  * @param algorithm  which compute an array from an array
	  * @param iterations number of steps
	  * @param size of the random array -> careful algorithm = O(n!)
	  * @return
	  */
	def computeAvgTime(algorithm: Seq[Int] => Seq[Int], iterations: Int = DEFAULT_NUMBER_OF_ITERATIONS,
			size: Int = DEFAULT_SIZE): Double = {
		var totalTime: Double = 0

		(1 to iterations).foreach(_ => {
			val randomArray: Seq[Int] = (1 to size).map(_ => util.Random.nextInt(50))
			val in_time = System.nanoTime()
			algorithm(randomArray)
			val out_time = System.nanoTime()
			totalTime += out_time - in_time
		})
		val avg = totalTime / (iterations * TO_MILLISECONDS)
		avg
	}
}

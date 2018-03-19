import common._

object Main {

	val TO_MILLISECONDS = 1000000f
	var finished = false

	def main(args: Array[String]): Unit = {
		val iterations = 1000
		val size = 7
		val randomArray = (1 to size).map(_ => util.Random.nextInt(50))

		val (sequentialTime, sorted1) = computeAvgTime(sequentialBogosort, iterations)(randomArray)
		val (parallelTime, sorted2) = computeAvgTime(parallelBogosort, iterations)(randomArray)

		println("Average Computing time :")
		println(s"Sequential implementation : $sequentialTime ms | $randomArray -> $sorted1")
		println(s"\nParallel implementation : $parallelTime ms | $randomArray -> $sorted2")
		println(s"\nOverrall speedup : " + sequentialTime / parallelTime)
	}

	def parallelBogosort(in: Seq[Int]): Seq[Int] = {

		def auxParallelBogosort(in: Seq[Int]): Seq[Int] = {
			var input = in
			var i = 1
			while (!finished && !isSorted(input)) {
				input = shuffle(input)
				i += 1
			}
			finished = true
			input
		}

		finished = false
		val output = parallel(auxParallelBogosort(in), auxParallelBogosort(in), auxParallelBogosort(in), auxParallelBogosort(in))
		if (isSorted(output._1)) output._1
		else if (isSorted(output._2)) output._2
		else if (isSorted(output._3)) output._3
		else output._4
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

	def computeAvgTime(sort: Seq[Int] => Seq[Int], iterations: Int)(array:Seq[Int]): (Double, Seq[Int]) = {
		var sorted = Seq[Int]()
		var totalTime: Double = 0

		(1 to iterations).foreach(_ => {
			val in_time = System.nanoTime()
			sorted = sort(array)
			val out_time = System.nanoTime()
			totalTime += out_time - in_time
		})
		val avg = totalTime / (iterations * TO_MILLISECONDS)
		(avg, sorted)
	}
}

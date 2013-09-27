package algorithms_class.greedy

object Scheduler extends App {
	
	type Job = (Long, Long) //weight, length
	
	def scheduleDiff(jobs: List[Job]): List[Job] =
		jobs.sortWith{case ((w1, l1), (w2, l2)) => {
			val d1 = (w1-l1)
			val d2 = (w2-l2)
			if (d1 == d2) w1 > w2
			else d1 > d2
			
		}}

	def scheduleRatio(jobs: List[Job]): List[Job] =
		jobs.sortBy{case (w, l) => w.toFloat/l.toFloat}.reverse
	
	//replace job length with job completion time
	def completion(jobs: List[Job], soFar: Long = 0, acc: List[Job] = Nil): List[Job] = jobs match {
		case (w, l) :: t => val r = l + soFar; completion(t, r, (w, r) :: acc) 
		case Nil => acc.reverse
	}
	
	def weightedCompletion(jobs: List[Job]): Long = completion(jobs).map{case (w, c) => w*c}.sum
	
	def parse(lines: List[String]): List[Job] = lines.drop(1).map(_.split(" ")).map{case Array(w,l) => (w.toLong, l.toLong)}
	
	val jobs: List[Job] = parse(io.Source.fromURL("http://spark-public.s3.amazonaws.com/algo2/datasets/jobs.txt").getLines.toList)	
	
	
	assert(69119377652L == weightedCompletion(scheduleDiff(jobs)))
	assert(67311454237L == weightedCompletion(scheduleRatio(jobs)))

}
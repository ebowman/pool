package pool

case class Match(hire: Hire, head: Head)

case class Bid(heads: Set[Head]) {
  def apply(hire: Hire): Match = {
    val h = heads.toSeq
    val probSum = h.map(_.pitchEfficiency).sum
    val scale = 1d / probSum
    val f = h.foldLeft(Seq.empty[(Head, Double)]) { case (seq, cur) =>
      if (seq.isEmpty) Seq((cur, cur.pitchEfficiency * scale))
      else seq :+ (cur, seq.last._2 + cur.pitchEfficiency * scale)
    }
    val rnd = util.Random.nextDouble()
    val winner = f.find(_._2 < rnd).getOrElse(f.last)._1
    Match(hire, winner)
  }
}

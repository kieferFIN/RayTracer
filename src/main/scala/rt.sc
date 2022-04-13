val v = (1 to 10).toVector
v.tails.map(_.take(2)).foreach(d=>println(s"$d #"))


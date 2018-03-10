package proteus.util

object TimeHelper {

  def sort(dbSlots: List[DbSlot]): List[DbSlot] = {
    dbSlots.sortWith { (a, b) =>
      duration(a.time) > duration(b.time)
    }
  }

  def duration(times: String): Int = {
    val eons = asInts(times)

    eons(1) - eons(0)
  }

  def asInts(times: String): Seq[Int] = {
    times.split("-").map { t => asInt(t)}
  }

  def asInt(time: String): Int = {
    val len = time.length
    val base = time.substring(0,len-2).replaceAll(":","").toInt
    val meridian = if(base < 1200 && time.endsWith("pm")) 1200 else 0
    base + meridian
  }
}

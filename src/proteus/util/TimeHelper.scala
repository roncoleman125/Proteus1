/*
 * Copyright (c) Ron Coleman
 * See CONTRIBUTORS.TXT for a full list of copyright holders.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the Scaly Project nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE DEVELOPERS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package proteus.util

/**
  * This object provides convenience methods for manipulating time in the schedule.
  */
object TimeHelper {
  /**
    * Sorts the db slots into decreasing order of time.
    * @param dbSlots Db slots
    * @return Reverse time sorted db slots
    */
  def sort(dbSlots: List[DbSlot]): List[DbSlot] = {
    dbSlots.sortWith { (a, b) =>
      duration(a.time) > duration(b.time)
    }
  }

  /**
    * Calculates the duration of a time in the form, e.g., 12:00pm-01:15pm
    * @param clocks
    * @return
    */
  def duration(clocks: String): Int = {
    val times = asInts(clocks)

    times(1) - times(0)
  }

  /**
    * Gets the clocks as an array of ints,  e.g., 12:00pm-01:15pm, becomes 1200 and 1315
    * @param clocks Clock times
    * @return
    */
  def asInts(clocks: String): Seq[Int] = {
    clocks.split("-").map { t => asInt(t)}
  }

  /**
    * Gets a clock, .g., 01:15pm as 1315.
    * @param clock Clock time
    * @return
    */
  def asInt(clock: String): Int = {
    // To cut out the am, pm
    val len = clock.length

    val base = clock.substring(0,len-2).replaceAll(":","").toInt

    val noon = if(clock.endsWith("pm") && base < 1200) 1200 else 0

    base + noon
  }
}

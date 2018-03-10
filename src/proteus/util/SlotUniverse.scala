package proteus.util

import proteus.casa.{MongoConnection, MongoDbObject, MongoHelper}

import scala.collection.mutable.ListBuffer
import org.bson.types.ObjectId


/**
 * @author Ron.Coleman
 */
object SlotUniverse {
  case class GridSlot(number: String, days: String, times: List[String])
  
  val slots = List(
    GridSlot("1", "MR", List("08:00am-09:15am")),
    GridSlot("2", "MR", List("09:30am-10:45am")),
    GridSlot("3", "TF", List("08:00am-09:15am")),
    GridSlot("4", "TF", List("09:30am-10:45am")),
    GridSlot("5", "WF", List("08:00am-09:15am", "11:00am-12:15pm")),
    GridSlot("6", "MW", List("11:00am-12:15pm", "09:30am-10:45")),
    GridSlot("7", "TR", List("11:00am-12:15pm")),
    GridSlot("8", "MR", List("12:30pm-01:45pm")),
    GridSlot("9", "TF", List("12:30pm-01:45pm")),
    GridSlot("10", "MR", List("02:00pm-03:15pm")),
    GridSlot("11", "TF", List("02:00pm-03:15pm")),
    GridSlot("12", "WF", List("02:00pm-03:15pm", "03:30pm-04:45pm")),
    GridSlot("13", "MW", List("03:30pm-04:45pm")),
    GridSlot("14", "TR", List("03:30pm-04:45pm")),
    GridSlot("15", "MW", List("05:00pm-06:15pm")),
    GridSlot("16", "TR", List("05:00pm-06:15pm")),
    GridSlot("17", "MW", List("06:30pm-07:45pm")),
    GridSlot("18", "TR", List("06:30pm-07:45pm")),
    GridSlot("19", "MW", List("08:00pm-09:15pm")),
    GridSlot("20", "T", List("08:00pm-09:15pm")),
    
    GridSlot("21", "M", List("06:30pm-09:15pm")),
    GridSlot("21", "M", List("05:00pm-09:15pm")),
    GridSlot("21", "M", List("06:30pm-09:00pm")),
    GridSlot("21", "M", List("06:30pm-08:50pm")),
    
    GridSlot("22", "T", List("06:30pm-09:15pm")),
    GridSlot("22", "T", List("05:00pm-09:15pm")),
    GridSlot("22", "T", List("06:30pm-09:00pm")),
    GridSlot("22", "T", List("06:30pm-08:50pm")),
    
    GridSlot("23", "W", List("06:30pm-09:15pm")),
    GridSlot("23", "W", List("05:00pm-09:15pm")),
    GridSlot("23", "W", List("06:30pm-09:00pm")),
    GridSlot("23", "W", List("06:30pm-08:50pm")),
    
    GridSlot("24", "R", List("06:30pm-09:15pm")),
    GridSlot("24", "R", List("05:00pm-09:15pm")),
    GridSlot("24", "R", List("06:30pm-09:00pm")),
    GridSlot("24", "R", List("06:30pm-08:50pm")),
    
    GridSlot("25", "S", List("08:00am-09:15am")),
    GridSlot("26", "S", List("11am-12:15pm")),
    GridSlot("27", "S", List("02:00pm-03:15pm")),
    GridSlot("28", "M", List("online")),
    GridSlot("29", "T", List("online")),
    GridSlot("30", "W", List("online")),
    GridSlot("31", "R", List("online")),
    GridSlot("32", "F", List("online")),
    GridSlot("33", "M", List("nonstandard")),
    GridSlot("34", "T", List("nonstandard")),
    GridSlot("35", "W", List("nonstandard")),
    GridSlot("36", "R", List("nonstandard")),
    GridSlot("37", "F", List("nonstandard"))
  )
    

  val dbSlots = new ListBuffer[DbSlot]()

  def main(args: Array[String]): Unit = {
    load
  }

  def load(): ListBuffer[DbSlot] = {
    val host = MongoHelper.host

    val port = MongoHelper.port

    val redshirt = MongoConnection(host, port)("redshirt")

    val slotsCollection = redshirt("slots")

    slots.foreach { slot =>
      val days = slot.days
      (0 until days.length).foreach { k =>
        val day = days(k) + ""
        
        val timeIndex = Math.min(k, slot.times.length - 1)
        val time = slot.times(timeIndex)
        
        // Classes that meet 1 day/week have no day indicator, only the slot number
        val slotNo = if(days.length == 1) slot.number else slot.number + day

        val cursor = slotsCollection.find(MongoDbObject("slotNo" -> slotNo))

        val list = MongoHelper.toList(cursor)

        if (list.size != 0) {
          val _id = list(0)("_id")
          
          dbSlots.append(DbSlot(slotNo, day, time, new ObjectId(_id)))
        }
        else
          println("did not find " + slotNo) 
      }
    }

    dbSlots
  }
}
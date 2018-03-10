package proteus

import proteus.casa.{MongoConnection, MongoDbObject, MongoHelper}
import proteus.util.{DbSlot, SlotUniverse, UserUniverse}

import scala.collection.mutable.ListBuffer

/**
 * This class exports the schedule as csv given a year and semester.
 * @author Ron.Coleman
 */
object ExportSchedule {
  val host = MongoHelper.host
  val port = MongoHelper.port
  val redshirt = MongoConnection(host, port)("redshirt")
  val coursesCollection = redshirt("courses")
  
  def main(args: Array[String]): Unit = {
    val year:Int = args(0).toInt
    val semester: String = args(1)
    
    // Load the courses from the db
    val cursor = coursesCollection.find(MongoDbObject("year" -> year, "semester" -> semester))

    val courses = MongoHelper.toList(cursor)
    // Get the users and slots as they are in the db
    UserUniverse.load
    
    SlotUniverse.load
    
    val last = true
    val header =
      field("Select") +
        field("CRN") +
        field("Subj") +
        field("Crse") +
        field("Sec") +
        field("Cmp") +
        field("Credits") +
        field("Title") +
        field("Days") +
        field("Time") +
        field("Cap") + 
        field("Act") + 
        field("Rem") + 
        field("XL Cap") + 
        field("XL Act") + 
        field("XL Rem") + 
        field("Instructor") +
        field("Date (MM)") +
        field("Location") +
        field("Comment", last)
    println(header)
    //println(courses.size)       
    // Map each course 
    courses.foreach { course =>
      val catalogName = course("catalogName")
      val offset = if(catalogName.contains("FYS")) -1 else 0
      val subj = catalogName.substring(0,4)
      val crse = catalogName.substring(4,8)
      val sec = catalogName.substring(8,11)
      val title = course("title")
      val cred = course("credits")
      val crn = course("crn")
      val owner = course("owner")
      val comment = if(course.contains("comment")) course("comment") else ""
      
      // Course might not have any slots which means it is not owned
      if (course.contains("slots")) {
        // value of "slots" key is "[id1, id2, id3]": convert to an array strings of ids
        val slots = asArray(course("slots"))
      
        // Convert the ids to slots that are in the database
        val dbSlots = lookupSlots(slots)
        
        val record =
            field("C") +
            field(crn) +
            field(subj) +
            field(crse) +
            field(sec) +
            field("AAA") +
            field(cred) +
            field(title) +
            fieldDays(dbSlots) +
            fieldTimes(dbSlots) +
            field(0) + // Cap
            field(0) + // Act
            field(0) + // Rem
            field(0) + // XL Cap
            field(0) + // XL Act
            field(0) + // XL Rem
            fieldOwner(owner) +
            field("") +
            field("")+
            field(comment, last)
         println(record)

      }
      // There is NO time slot so assume course is TBA
      else {
        val record =
            field("C") +
            field(crn) +
            field(subj) +
            field(crse) +
            field(sec) +
            field("AAA") +
            field(cred) +
            field(title) +
            field("TBA") +         // Days
            field("TBA") +         // Time
            field(0) +             // Cap
            field(0) +             // Act
            field(0) +             // Rem
            field(0) +             // XL Cap
            field(0) +             // XL Act
            field(0) +             // XL Rem
            field("TBA") +         // Instructor
            field("") +
            field("") +
            field("", last)        // Comment
        println(record)
      }
      
      // If course has lab, it has days / times like the lecture
      if(course.contains("lab")) {
        val slot = course("lab")
        
        val dbSlots = lookupSlots(Array(slot))
        
        val record2 =
          fieldsSkip(7+1) +
          fieldDays(dbSlots) +
          fieldTimes(dbSlots) +
          fieldsSkip(6) + 
          fieldOwner(owner) +
          field("") +
          field("", last)
        println(record2)
      }
    }
  }
  
  /** Assumes s is [e1, e2, e3, ...] which we convert an arry if strings
   *  1 and -1 step over [ and ] */
  def asArray(s: String): Array[String] = s.substring(1,s.length-1).split(",").map { e => e.trim }
  
  /** Creates number field. */
  def field(n: Int) = n.toString + ","
  
  /** Skips n fields */
  def fieldsSkip(n: Int): String = (for(i <- (0 until n)) yield ",").mkString
  
  /** Creates string field. */
  def field(s: String, last: Boolean = false) = if(!last) s + "," else s
  
  /** Create field for days from a slot. */
  def fieldDays(slots: List[DbSlot]): String = {
    val days = slots.length match {
      case 2 =>
        slots(0).day + slots(1).day
        
      case 1 =>
        slots(0).day
    }
    
    days + ","
  }
  
  /** Create field for time. */
  def fieldTimes(slots: List[DbSlot]): String = {
    slots(0).time + ","
  }
  
  /** Creates field for owner. */
  def fieldOwner(id: String): String = {
    UserUniverse.dbUsers.find { user => user.id.toString == id } match {
      case Some(user) =>
        user.fullname + ","
        
      case None =>
        "UNKNOWN,"
    }
  }
  
  /** Converts array list of ids to list of db slots */
  def lookupSlots(ids: Array[String]): List[DbSlot] = {
    val dbSlots = ListBuffer[DbSlot]()

    ids.foreach { id =>
      SlotUniverse.dbSlots.find { slot => slot.objid.toString == id } match {
        case Some(slot) =>
          dbSlots.append(slot)
          
        case None =>
          Console.err.println("bad slot id " + id)
      }
    }
    
    dbSlots.toList
  }
}
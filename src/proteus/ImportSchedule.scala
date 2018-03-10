package proteus

import java.util.Date

import scala.collection.mutable.ListBuffer
import scala.io.Source
import org.bson.types.ObjectId
import proteus.casa.MongoConnection
import proteus.casa.MongoDbObject
import proteus.casa.MongoHelper
import proteus.util.{DbSlot, DbUser, SlotUniverse, UserUniverse}

import scala.util.Random

/**
  * This objects reads and spreadsheet and load the courses with pre-assigned users.
  * @author Ron.Coleman
  */
object ImportSchedule {
  import proteus.util.SpreadsheetColumn._
  val host = MongoHelper.host
  val port = MongoHelper.port
  val redshirt = MongoConnection(host, port)("redshirt")

  val coursesCollection = redshirt("courses")

  val ran = new Random(0)

  val MULTIDAY_SEPERATOR = ";"

  def main(args: Array[String]): Unit = {
    // Test
    // Get the semester, year, and the courses
    val year = args(0).toInt

    val semester = args(1)

    val records = loadSheet(args(2))

    // Load the slots and users
    SlotUniverse.load
    UserUniverse.load

    // Try to match the days / time with the database
    //    val _records = List("C,11497,CMPT,120L,111,AAA,4,INTRO TO PROGRAMMING,TF;R,12:30 pm-01:45 pm;09:30 am-10:45 am,25,30,-5,0,0,0,Cathy E Martensen (P),01/16-05/11,HC 1021,,")
    //    val (identified, nonidentified) = analyzeSlots(_records)
    val (identified, nonidentified) = analyzeSlots(records)

    println("records = "+records.length)
    println("standard = "+identified.size)
    println("online and nonstandard = "+nonidentified.size)
    val onlineCount = nonidentified.foldLeft(0) { (count, record) =>
      val fields = record.split(",")
      if(fields(TIMES).trim == "TBA") count+1 else count
    }
    println("onlines = "+onlineCount)
    println("nonstandards = "+(nonidentified.size - onlineCount))

    assign(year, semester, identified, records)
  }

  /** Assign each course to a time slot and insert into database */
  def assign(year: Int, semester: String, identifieds: List[(String, List[DbSlot])], records: List[String]): Unit = {
    // First delete all documents for the semester
    val result = coursesCollection.deleteMany(MongoDbObject("year" -> year, "semester" -> semester))
    println("deleted="+result.getDeletedCount)

    // Load the user data from the database
    val dbUsers = UserUniverse.dbUsers

    // TBA assumed to be at this index in users.txt
    val INDEX_TBA = 0

    val tbaUser = dbUsers(INDEX_TBA)

    // Get the online and nonstandard db slots
    val onlineSlots = SlotUniverse.dbSlots.filter { slot => slot.time == "online"}.toList
    val nonstandarSlots = SlotUniverse.dbSlots.filter { slot => slot.time == "nonstandard"}.toList

    // Insert the courses in the order they appear in the import CSV
    records.foreach { record =>
      val fields = asArray(record)

      val catalogName = fields(SUBJ) + fields(CRSE) + fields(SEC)
      val days = fields(DAYS)
      val time = fields(TIMES)

      // Get the instructor without the extraneous text
      val instructor = fields(INSTRUCTOR).replaceAll(" \\(P\\)", "")

      identifieds.find { element =>
        val (rec, dbSlots) = element

        record == rec
      } match {
        case Some(identifed) =>
          // IDENTIFIED time slot
          val (_, dbSlots) = identifed

          // Find the Gerty user that corresponds to the instructor
          dbUsers.find { user => instructor.startsWith(user.fullname) } match {
            case Some(user) =>
              // F/T faculty
              println(instructor + " (f/t) => "+catalogName+" "+days+" "+time+" STAND")
              insert(year, semester, user, dbSlots, record)

            case None =>
              // Adjunct faculty
              println(instructor + " (adjunct) => "+catalogName+" "+days+" "+time+" STAND")
              insert(year, semester, tbaUser, dbSlots, record)

          }
        case None =>
          // NONSTANDARD time slot
          dbUsers.find { user => instructor.startsWith(user.fullname) } match {
            case Some(user) =>
              // F/T faculty
              println(instructor + " (f/t) => "+catalogName+" "+days+" "+time+" NON/ONL")
              insert(year, semester, user, Tuple2(onlineSlots, nonstandarSlots), record)

            case None =>
              // Adjunct faculty
              println(instructor + " (adjunct) => "+catalogName+" "+days+" "+time+" NON/ONL")
              insert(year, semester, tbaUser, Tuple2(onlineSlots, nonstandarSlots), record)
          }
      }
    }
  }

  /** Inserts a course given we know the user and slots */
  def insert(year: Int, semester: String, user: DbUser, dbSlots: List[DbSlot], record: String): Unit = {
    val now = new Date().getTime.toString

    val fields = asArray(record)

    val slotIds = ListBuffer[ObjectId]()

    dbSlots.foreach { dbSlot =>
      slotIds.append(dbSlot.objid)
    }

    val (lectureIds, labId) = getLectureLab(dbSlots)

    // Needed (below) to convert slotIds to a Java array
    import scala.collection.JavaConverters._

    val catalogName = fields(SUBJ) + fields(CRSE) + fields(SEC)

    val course = MongoDbObject(
      "year" -> year,
      "semester" -> semester,
      "title" -> fields(TITLE),
      "catalogName" -> catalogName,
      "crn" -> fields(CRN),
      "credits" -> fields(CREDS).toInt,
      "slots" -> lectureIds.asJava, //slotIds.asJava,
      "lab" -> labId,//null,
      "comment" -> null,
      "owner" -> user.id,
      "updated" -> now,
      "__v" -> -1)

    //    println(course)
    coursesCollection.insertOne(course)
  }

  def getLectureLab(dbSlots: List[DbSlot]): Tuple2[List[DbSlot],DbSlot] = {
    dbSlots.length match {
      case 1 =>
        (dbSlots, null)

      case 2 =>
        // If the slot numbers are the same, assume slots are for lecture.
        // if the slot numbers are not the same, make one the lecture and the other the lab.
        val num1 = dbSlots(0).getNumber
        val num2 = dbSlots(1).getNumber

        if(num1 == num2)
          (dbSlots, null)
        else {
          val lecture = dbSlots.take(1)(0)
          val lab = dbSlots(1)

          // We'll sort the slots since the order could be reverse in the CSV
          import proteus.util.TimeHelper._
          if(duration(lab.time) < duration(lecture.time))
            (dbSlots.take(1), dbSlots(1))
          else
            (dbSlots.drop(1), dbSlots(0))
        }
      case 3 =>
        // Assume two adjacent three will be same slot
        // In this case there's no need to sort them since...the lecture
        // slot will by definition be longer that the lab.
        val num1 = dbSlots(0).getNumber
        val num2 = dbSlots(1).getNumber
        val num3 = dbSlots(2).getNumber

        val (lecture, lab) =
          if(num1 == num2) (dbSlots.slice(0, 2), dbSlots(2))
          else /* num2 == num3 */ (dbSlots.drop(1), dbSlots(0))

        (lecture, lab)
      case _ =>
        println("WARNING unexpected number of db slots = "+dbSlots.length+" putting all in lecture!")
        (dbSlots, null)
    }
  }

  /** Inserts course into an "optimal" online or nonstandard slot */
  def insert(year: Int, semester: String, user: DbUser, offslots: (List[DbSlot],List[DbSlot]), record: String): Unit = {
    val (onlines, nonstandards) = offslots

    val fields = asArray(record)

    // Pick a time
    val slot = fields(TIMES) match {
      // All TBAs times will get a random online slot
      case "TBA" =>
        val lottery = ran.nextInt(onlines.size)
        val slot = onlines(lottery)
        slot

      case _ =>
        // If there is a time, pick a slot for the day, if there is a day.
        // If there isn't a day, randomly choose one.
        val optimalDay = if(fields(DAYS).length > 0) fields(DAYS)(0) + "" else ""

        nonstandards.find { slot => slot.day == optimalDay } match {
          case Some(slot) =>
            slot

          case None =>
            val lottery = ran.nextInt(nonstandards.size)
            val slot = nonstandards(lottery)
            slot
        }
    }

    val dbSlots = List(slot)
    insert(year, semester, user, dbSlots, record)
  }

  /* Returns list(identified record, corresponding db slots) AND list(nonidentified record) */
  def analyzeSlots(records: List[String]): (List[(String, List[DbSlot])], List[String]) = {
    // Get the slots
    val nonidentified = ListBuffer[String]()

    // Try to match up every record (String) with its slots (List[DbSlot]).
    val identifieds = records.foldLeft(ListBuffer[(String,List[DbSlot])]()) { (identified, record) =>
      val fields = asArray(record)

      val days = fields(DAYS).split(MULTIDAY_SEPERATOR)

      val times = fields(TIMES).replaceAll(" ","").split(MULTIDAY_SEPERATOR)

      // For each day / time, check the database for a match and return all the matches for this record
      val matches = (0 until days.length).foldLeft(ListBuffer[DbSlot]()) { (matchedSlots, k) =>
        days(k).foreach { dayChar =>
          val day = dayChar.toString

          // Select the one slot that should match here
          SlotUniverse.dbSlots.find { slot => day == slot.day && times(k).contains(slot.time) } match {
            case Some(slot) =>
              matchedSlots.append(slot)

            case None =>
              //              nonidentified.append(record)
              println("WARNING no slot match for days/times for course: "+record)
          }
        }

        matchedSlots
      }

      // Check if the matched slots are efficiently used
      if(matches.length > 0 && !isEfficient(matches.toList))
        println("WARNING inefficient use of slot for course: "+record)

      // Put the record and its slots in the identified set, otherwise put them in the nonidentified set
      if(matches.length != 0)
        identified.append((record, matches.toList))
      else
        nonidentified.append(record)

      identified
    }

    // Return the records whose slots we could identify and those records
    // whose slots we could not identify
    (identifieds.toList, nonidentified.toList)
  }

  /** Return records from the spreadsheet */
  def loadSheet(path: String): List[String] = {
    // Get the raw lines
    val rawLines = Source.fromFile(path, "UTF-8").getLines.toList

    // Merge the days & times when course has a lab
    val merged = merge(rawLines)

    // Get only CMPT, MSCS, MSIS, and DATA rows
    val records = rawLines.filter { line =>
      val fields = line.split(",")

      fields.length match {
        case len if len < 17 =>
          false

        case _ =>
          val subj = fields(SUBJ).trim

          subj match {
            case "CMPT" | "FYS" | "MSCS" | "MSIS" | "DATA" =>
              true

            case _ =>
              false
          }
      }
    }

    records
  }

  /** Merges multiple records where they are split across adjacent lines */
  def merge(records: List[String]): List[String] = {
    // All record we return stored here
    val store = ListBuffer[String]()

    // List record seen so far--this causes a blank record to be added to the store
    var lastRecord = Array[String]()

    // Go through each record and the incomplete ones we'll merge with a last record
    (0 until records.length).foreach { index =>
      val record = records(index)

      val fields = asArray(record)

      // If this record is complete by its title, store the old record and
      // repare current record for merging
      if(isComplete(fields(TITLE))) {
        val merged = asString(lastRecord)

        store.append(merged)

        lastRecord = asArray(record)
      }
      else {
        lastRecord(DAYS) = lastRecord(DAYS) + MULTIDAY_SEPERATOR + fields(DAYS)
        lastRecord(TIMES) = lastRecord(TIMES) + MULTIDAY_SEPERATOR + fields(TIMES)
      }
    }

    // The last record must still be added to the store
    val merged = asString(lastRecord)

    store.append(merged)

    // Drop first since initial lastRecord is empty
    store.toList.drop(1)
  }

  /** Makes the record array into a string */
  def asString(record: Array[String]) = record.foldLeft("") { (s, field) => s + field + "," }

  /** Makes the record string into an array of fields */
  def asArray(record: String) = record.split(",").map(_.trim)

  def isComplete(field: String) = field.length != 0

  /** Returns true if the number of identified slots equals the number days in grid */
  def isEfficient(dbSlots: List[DbSlot]): Boolean = {
    if(dbSlots.length == 0)
      false

    // Get the number part of the slotNo
    val slotNumber = dbSlots(0).slotNo.filter { c => c.isDigit }

    // Look for this number in the slot universe
    SlotUniverse.slots.find(slot => slot.number == slotNumber) match {
      case Some(slot) =>
        if(slot.days.length == dbSlots.length)
          true
        else
          false
      case None =>
        false
    }
  }
}

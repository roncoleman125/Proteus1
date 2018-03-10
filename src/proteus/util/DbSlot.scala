package proteus.util

import org.bson.types.ObjectId

/**
  * Representation of database slot.
  * @param slotNo Slot number
  * @param day Day code
  * @param time Time code
  * @param objid Object id in the database
  */
case class DbSlot(slotNo: String, day: String, time: String, objid: ObjectId) {
  def getNumber: String = {
    slotNo.filter { c => c.isDigit }
  }
}

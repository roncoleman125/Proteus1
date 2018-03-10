package proteus.util

import org.bson.types.ObjectId
import scala.io.Source
import scala.collection.mutable.ListBuffer
import proteus.casa.MongoConnection
import proteus.casa.MongoHelper
import proteus.casa.MongoDbObject

/**
 * @author Ron.Coleman
 */
case class DbUser(logname: String, fullname: String, id: ObjectId)

object UserUniverse {
  val dbUsers = new ListBuffer[DbUser]()
  
  def main(args: Array[String]): Unit = {
    load
  }
  
  def load(): List[DbUser] = {
    val host = MongoHelper.host

    val port = MongoHelper.port

    val redshirt = MongoConnection(host, port)("redshirt")

    val usersCollection = redshirt("users")
    
    val lines = Source.fromFile("./users.txt").getLines.toList
    
    lines.foreach { line =>
      val values = line.split(",")
      
      val name = values(0).trim
      val fullname = values(1).trim
      
      val cursor = usersCollection.find(MongoDbObject("name" -> name))
      
      val list = MongoHelper.toList(cursor)
      
      if(list.size != 0) {
        val id = list(0)("_id")
        
        dbUsers.append(DbUser(name, fullname, new ObjectId(id)))
      }
      else
        println("user "+name+" not found")
    }
    
    dbUsers.toList
  }
}
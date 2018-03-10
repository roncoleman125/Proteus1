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
package proteus.casa

import java.util.Date
import com.mongodb.client.FindIterable
import org.bson.Document
import org.bson.types.ObjectId
import scala.collection.mutable.{HashMap, ListBuffer}

/**
 * @author Ron.Coleman
 */
object MongoHelper {
  /** Converts mongo cursor to scala list of string objects */
  def asList(results: FindIterable[Document], field: String): List[String] = {
    val cursor = results.iterator
    
    if (cursor.hasNext) {
      val value = cursor.next().get(field)

      value match {
        case list: java.util.List[_] =>
          import scala.collection.JavaConverters._
          list.asInstanceOf[java.util.List[String]].asScala.toList

        case _ =>
          scala.List[String]()

      }
    }
    else
      List[String]()
  }
  
//  def toList(cursor: FindIterable[Document], field: String): List[String] = {
//    val iter = cursor.iterator
//    
//    val buffer = ListBuffer[String]()
//    
//    while(iter.hasNext) {
//      val obj = iter.next
//      
//      val doc = iter.next
//      
//      doc.get(field) match {
//        case s: String =>
//          buffer.append(s)
//          
//        case i: java.lang.Integer =>
//            buffer.append(i.toString)
//            
//        case o: ObjectId =>
//            buffer.append(o.toString)
//            
//        case _ =>
//      }
//    }
//    
//    buffer.toList
//  }

  def toList(cursor: FindIterable[Document], field: String = null): List[HashMap[String,String]] = {
    val iter = cursor.iterator
    
    val buffer = ListBuffer[HashMap[String,String]]()
    
    while(iter.hasNext) {
      val doc = iter.next

      import scala.collection.JavaConverters._
      val keys = if(field == null) doc.keySet.asScala else doc.keySet.asScala.filter { f => f == field }
      
      val map = HashMap[String,String]()
      
      keys.foreach { k =>
        val obj = doc.get(k)
        
        obj match {
          case str: String =>
            map(k) = str
            
          case i: java.lang.Integer =>
            map(k) = i.toString
            
          case ob: ObjectId =>
            map(k) = ob.toString
            
          case array: java.util.ArrayList[Object] =>
            map(k) = array.toString()
            
          case _ =>
        }
      }
      buffer.append(map)
    }
    
    buffer.toList
  }
  
  /** Convert [a,b,c] to List(a,b,c). Does NOT handle nested arrays e.g., [a,[b,c],d]. */
  def toList(s: String): ListBuffer[String] = {
    val list = ListBuffer[String]()
    
    val elements = s.replaceAll("\\[", "").replaceAll("\\]","").split(",")
    
    elements.foreach { element =>
      list.append(element.trim)
    }
    
    list
  }
  
  /** Augments user with Mongoose required fields of Gerty model */
  def augmentUser(args: (String,Any)*): Document = {
    val now = new Date().getTime.toString
    val doc = MongoDbObject("lastLogin" -> now, "created" -> now, "__v" -> -1)
    args.foldLeft(doc) { (doc,arg) =>
      val (key, value) = arg
      doc.append(key,value)
    }
  }
  
  val host: String = getHost
  
  def getHost: String = {
    val host = System.getenv("MONGO_HOST")
    
    if(host != null) host else "127.0.0.1"
  }
  
  val port: Int = getPort
  
  def getPort: Int = {
    val port = System.getenv("MONGO_PORT")
    
    if(port != null) port.toInt else 27017
  }
}
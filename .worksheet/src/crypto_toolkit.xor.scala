package crypto_toolkit

import scala.math._
import scala.collection.BitSet

/*
PRODUCED WRONG ANSWER :(
*/


object xor {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(250); 

  def xor(x: Array[Byte], y: Array[Byte]): Array[Byte] = x.zip(y).map(_ match { case (a: Byte, b: Byte) => (a ^ b).toByte });System.out.println("""xor: (x: Array[Byte], y: Array[Byte])Array[Byte]""");$skip(146); 

  def hex2bytes(hex: String): Array[Byte] = {
    hex.replaceAll("[^0-9A-Fa-f]", "").grouped(2).toArray.map(Integer.parseInt(_, 16).toByte)
  };System.out.println("""hex2bytes: (hex: String)Array[Byte]""");$skip(252); 

  def bytes2hex(bytes: Array[Byte], sep: Option[String] = None): String = {
    sep match {
      case None => bytes.map("%02x".format(_)).mkString
      case _ => bytes.map("%02x".format(_)).mkString(sep.get)
    }
    // bytes.foreach(println)
  };System.out.println("""bytes2hex: (bytes: Array[Byte], sep: Option[String])String""");$skip(70); 

  val m1 = BigInt("2199677439761906166135512011931981").toByteArray;System.out.println("""m1  : Array[Byte] = """ + $show(m1 ));$skip(53); 
  val c1 = hex2bytes("6c73d5240a948c86981bc294814d");System.out.println("""c1  : Array[Byte] = """ + $show(c1 ));$skip(27); val res$0 = 

  (m1.length, c1.length);System.out.println("""res0: (Int, Int) = """ + $show(res$0));$skip(64); 
  //m1 xor k1 => c1
  //k1 = m1 xor c1

  val k1 = xor(m1, c1);System.out.println("""k1  : Array[Byte] = """ + $show(k1 ));$skip(55); 

  val m2 = hex2bytes("61747461636B206174206475736B");System.out.println("""m2  : Array[Byte] = """ + $show(m2 ));$skip(38); 


  println(bytes2hex(xor(k1, m2)))}

}

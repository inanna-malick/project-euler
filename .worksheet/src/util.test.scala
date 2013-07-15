package util

/*
On input 064 the output is "9d1a4f78 cb28d863".    On input 132 032 the output is "75e5e3ea 773ec3e6".
On input 064 the output is "7c2822eb fdc48bfb".    On input 132 032 the output is "325032a9 c5e2364b".
On input 064 the output is "5f67abaf 5210722b".    On input 132 032 the output is "bbe033c0 0bc9330e".
On input 064 the output is "290b6e3a 39155d6f".    On input 132 032 the output is "d6f491c5 b645c008".
*/

import java.lang.Long

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(517); 
  val l_in: Long = Long.decode("0x9d1a4f78");System.out.println("""l_in  : Long = """ + $show(l_in ));$skip(45); 
  val r_in: Long = Long.decode("0xcb28d863");System.out.println("""r_in  : Long = """ + $show(r_in ));$skip(48); 

  val l_out: Long = Long.decode("0x75e5e3ea");System.out.println("""l_out  : Long = """ + $show(l_out ));$skip(46); 
  val r_out: Long = Long.decode("0x773ec3e6");System.out.println("""r_out  : Long = """ + $show(r_out ));$skip(30); val res$0 = 

  Long.toBinaryString(r_in);System.out.println("""res0: String = """ + $show(res$0));$skip(35); val res$1 = 
  "0" + Long.toBinaryString(r_out);System.out.println("""res1: String = """ + $show(res$1))}

}

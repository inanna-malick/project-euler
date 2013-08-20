package complete._0to99
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.RegexParsers
/*
If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, 
how many letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two)  
contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. 
The use of "and" when writing out numbers is in compliance with British usage.
 */



class Parser extends RegexParsers {
	val ones = Map("0" -> "zero", "1" -> "one", 
						 "2" -> "two", "3" -> "three", 
						 "4" -> "four", "5" -> "five", 
						 "6" -> "six", "7" -> "seven", 
						 "8" -> "eight", "9" -> "nine")
						 
	val tens = Map("2" -> "twenty", "3" -> "thirty", 
						 "4" -> "forty", "5" -> "fifty", 
						 "6" -> "sixty", "7" -> "seventy", 
						 "8" -> "eighty", "9" -> "ninety")
						 
	val teens = Map("10" -> "ten", "11" -> "eleven", 
						 "12" -> "twelve", "13" -> "thirteen", 
						 "14" -> "fourteen", "15" -> "fifteen", 
						 "16" -> "sixteen", "17" -> "seventeen",
						 "18" -> "eighteen", "19" -> "nineteen")
						 
	def one: Parser[String] = """\d""".r ^^ { ones(_) }
	
	def ten: Parser[String] = """\d""".r ^^ { tens(_)}
	
	def hundred: Parser[String] = """\d""".r ^^ { ones(_) + " hundred and "}

	def thousand: Parser[String] = """\d""".r ^^ { ones(_) + " thousand "}
	
	def teen: Parser[String] = """1\d""".r ^^ { teens(_)}
	
	
	//What type is this?!!!!!!!!!
	def number = thousand ~ hundred ~ (teen | (ten ~ one))
}

object Problem17 extends Parser {
	
	

	def main(args: Array[String]): Unit = {
		
		val s = "1112"
		
		//we need to wrap the string in a reader so our parser can digest it
		val input = new CharSequenceReader(s)
		val result = number(input) match {
			case Success(t, _) => println(t); t
			case NoSuccess(msg, _) => throw new IllegalArgumentException(
				"Could not parse '" + s + "': " + msg)
		}
		
		
		
	}

	
	
	
	
	
	
}















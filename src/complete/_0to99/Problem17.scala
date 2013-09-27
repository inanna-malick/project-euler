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
	val ones = Map("0" -> "", "1" -> "one",
		"2" -> "two", "3" -> "three",
		"4" -> "four", "5" -> "five",
		"6" -> "six", "7" -> "seven",
		"8" -> "eight", "9" -> "nine")

	val tens = Map("0" -> "",
		"2" -> "twenty", "3" -> "thirty",
		"4" -> "forty", "5" -> "fifty",
		"6" -> "sixty", "7" -> "seventy",
		"8" -> "eighty", "9" -> "ninety")

	val teens = Map("10" -> "ten", "11" -> "eleven",
		"12" -> "twelve", "13" -> "thirteen",
		"14" -> "fourteen", "15" -> "fifteen",
		"16" -> "sixteen", "17" -> "seventeen",
		"18" -> "eighteen", "19" -> "nineteen")
						 
	def one: Parser[String] = """\d""".r ^^ {ones(_)}
	
	def ten: Parser[String] = """[02-9]""".r ^^ {tens(_)}
	
	def hundred: Parser[String] = """\d""".r ^^ {ones(_)}

	def thousand: Parser[String] = """\d""".r ^^ {ones(_)}
	
	def teen: Parser[String] = """1[0-9]""".r ^^ {teens(_)}
	
	def _1:Parser[String] = one
	def _2a: Parser[String] = (ten ~ one) ^^ {case a ~ "" => a
											  case "" ~ b => b
											  case a ~ b => a + " " + b}
	
	def _2: Parser[String] = teen | _2a
	
	def _3: Parser[String] = (hundred ~ _2) ^^ {case "" ~ b => b
												case a ~ "" => a + " hundred"
												case a ~ b => a + " hundred and " + b}
	
	def _4: Parser[String] = (thousand ~ _3) ^^ {case "" ~ b => b
												case a ~ "" => a + " thousand"
												 case a ~ b => a + " thousand and " + b}
	
	def number: Parser[String] = _4 | _3 | _2 | _1
}

object Problem17 extends Parser {
	
	def parse(s: String): Int = {
		//wrap the string in a reader so our parser can digest it
		val input = new CharSequenceReader(s)
		val result = number(input) match {
			case Success(t, _) => println(t); t
			case NoSuccess(msg, _) => throw new IllegalArgumentException(
				"Could not parse '" + s + "': " + msg)
		}
		result.count(('a' to 'z').contains(_))
	}
	
	//is it inefficient and unnecessary to parse string representations 
	//of numbers into the full British form using parsers? You betcha. 
	def main(args: Array[String]): Unit = {
		assert((1 to 1000).map(n => parse(n.toString)).sum == 21124)		
	}

	
	
	
	
	
	
}















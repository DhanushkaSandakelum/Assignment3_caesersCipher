object caesar_cipher_imperative extends App
{
	val plain = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	val plainArray = plain.toArray
	val characterAmount = 26
	
	//GET CURRENT CHARACTER POSITION WITH RESPECT TO THE PLAIN TEXT
	def getPositionIndex(ch: Char): Int =
	{
		var pos = 0
		for(i <- 0 until characterAmount)
		{
			if(ch == plain(i))
				pos = i
		}
		
		pos
	}
	
	//ENCRYPTION
	def encrypt(text: String, n: Int): String =
	{
		var out = text.toArray
		
		for(i <- 0 until text.length)
		{
			if(text(i) != ' ')
			{
				var e = (getPositionIndex(text(i)) + n) % characterAmount
				out(i) = plain(e)
			}
		}
		
		out.mkString("")
	}
	
	//DECRYPTION
	def decrypt(text: String, n: Int): String =
	{
		var out = text.toArray
		
		for(i <- 0 until text.length)
		{
			if(text(i) != ' ')
			{
				var x = getPositionIndex(text(i)) - n
				if (x < 0)
					x = characterAmount - x.abs
				else
					x = x
					
				var d = x % characterAmount
				out(i) = plain(d)
			}
		}
		
		out.mkString("")
	}
	
	def plainToCipher(): Unit =
	{
		println("enter plain text> ")
		val text = scala.io.StdIn.readLine()
		println("enter shift> ")
		val shift = scala.io.StdIn.readInt()
		
		println("\ncipher text -->")
		println(encrypt(text, shift))		
	}
	
	def cipherToPlain(): Unit =
	{
		println("enter cipher text> ")
		val text = scala.io.StdIn.readLine()
		println("enter shift> ")
		val shift = scala.io.StdIn.readInt()

		println("\nplain text -->")
		println(decrypt(text, shift))
	}
	
	def cipherToPlainAutomaticPossibibleCases(): Unit =
	{
		println("enter cipher text> ")
		val text = scala.io.StdIn.readLine()
		
		println("\nall possible cases --> ")
		for(i <- 0 until characterAmount)
		{
			println("case " + i + ": " + decrypt(text, i))
		}
	}
	
	def cipher(): Unit =
	{
		var run = true
		
		while(run)
		{
			println("\nCAESER'S CIPHER")
			println("1. Enter plain text to encrypt as cipher text")
			println("2. Enter cipher text to decrypt as plain text")
			println("3. Enter ciper text to automatic decrypt all possible cases")
			println("0. Exit")
			println("option> ")
			var option = scala.io.StdIn.readInt()
		
			option match{
				case 1 => plainToCipher()
				case 2 => cipherToPlain()
				case 3 => cipherToPlainAutomaticPossibibleCases()
				case 0 => run = false
				case _ => println("invalid option !")
			}
		}
	}
	
	cipher()	
}
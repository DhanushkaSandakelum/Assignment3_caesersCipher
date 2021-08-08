object caesar_cipher_functional extends App {
	//Define alphabet
	val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWZYZ"

	//Define encryption annonymous function
	val E = (c: Char, key: Int, a: String) => a((a.indexOf(c.toUpper) + key) % a.size)

	//Define decryption annonymous function
	val D = (c: Char, key: Int, a: String) => a((a.indexOf(c.toUpper) - key) % a.size)

	//cippher annonymous function
	val cipher = (method: (Char, Int, String) => Char, s: String, key: Int, a: String) => s.map(method(_, key, a))

	//ciphering
	val  default_text = "Dhanushka"
	val cipher_text = cipher(E, default_text, 3, alphabet)
	val plain_text = cipher(D, cipher_text, 3, alphabet)

	println("Default text: " + default_text)
	println("Cipher text : " + cipher_text)
	println("Plain text  : " + plain_text)
}
object Q1 {

  def cipher(word: String, shift: Int, f: (String, Int) => String): String = {
    f(word, shift)
  }

  def encryption(word: String, shift: Int): String = {
    val encryptedWord = new StringBuilder

    for (i <- 0 until word.length) {
      val inputLetter = word.charAt(i)
      val outputLetter = if (inputLetter.isLetter) {
        val ASCII_value = if (inputLetter.isUpper){
          65   //if it is a capital letter
        }
        else {
          97  //if it is a simple letter
        }
        val calculateValue = (inputLetter - ASCII_value + shift) % 26 + ASCII_value  //get the ASCII value of the inputted letter
        calculateValue.toChar
      }
      else inputLetter
      encryptedWord.append(outputLetter)
    }
    encryptedWord.toString()
  }

  def decryption(word: String, shift: Int): String = {
    val decryptedWord = new StringBuilder

    for (i <- 0 until word.length) {
      val inputLetter = word.charAt(i)
      val outputLetter = if (inputLetter.isLetter) {
        val ASCII_value = if (inputLetter.isUpper){
          65
        }else{
          97
        }
        val calculateValue = (inputLetter - ASCII_value - shift + 26) % 26 + ASCII_value
        calculateValue.toChar
      }
      else inputLetter
      decryptedWord.append(outputLetter)
    }
    decryptedWord.toString()
  }

  def main(args: Array[String]): Unit = {
    print("Enter the word: ")
    val word = scala.io.StdIn.readLine()

    print("Enter the shift value: ")
    val shift = scala.io.StdIn.readInt()

    val encryptedWord = cipher(word, shift, encryption)
    println("Encrypted Word: " + encryptedWord)

    val decryptedWord = cipher(word, shift, decryption)
    println("Decrypted Word: " + decryptedWord)
  }
}
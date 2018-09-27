package fintech.homework01

// Используя функции io.readLine и io.printLine напишите игру "Виселица"
// Пример ввода и тест можно найти в файле src/test/scala/fintech/homework01/HangmanTest.scala
// Тест можно запустить через в IDE или через sbt (написав в консоли sbt test)

// Правила игры "Виселица"
// 1) Загадывается слово
// 2) Игрок угадывает букву
// 3) Если такая буква есть в слове - они открывается
// 4) Если нет - рисуется следующий элемент висельника
// 5) Последней рисуется "веревка". Это означает что игрок проиграл
// 6) Если игрок все еще жив - перейти к пункту 2

// Пример игры:

// Word: _____
// Guess a letter:
// a
// Word: __a_a
// Guess a letter:
// b
// +----
// |
// |
// |
// |
// |

// и т.д.

class Hangman(io: IODevice) {

  private var errorAttempts = Set[Char]()
  private var gallowsStage = -1
  private var guessedLetters = ""

  private def checkLetter(word: String, letter: Char): Set[Int] = {
    var positions = Set[Int]()
    for (i <- Range(0, word.length)) if (word.charAt(i) == letter) positions += i
    positions
  }

  private def processLetter(word : String, letter : Char): String = {
    if (errorAttempts.contains(letter)) return "You have already typed this letter"
    errorAttempts += letter
    val positions = checkLetter(word, letter)
    if (positions.isEmpty) gallowsStage += 1
    else for (i <- positions)
      guessedLetters = guessedLetters.substring(0, i) + letter + guessedLetters.substring(i + 1, guessedLetters.length)
    val stage = if (gallowsStage > -1) stages(gallowsStage) else ""
    stage
  }

  def play(word: String): Unit = {
    guessedLetters = "_" * word.length
    while (gallowsStage < stages.length - 1 && guessedLetters != word) {
      io.printLine("Word: " + guessedLetters + "\nGuess a letter:")
      val letter = io.readLine()
      if(letter.length == 1){
        val answer = processLetter(word, letter.charAt(0))
        io.printLine(answer)
      }
      else io.printLine("Enter only one letter")
    }
    io.printLine(if (gallowsStage == stages.length - 1) "You are dead" else "You win. The word is " + word)
  }

  val stages = List(
    """+----
      ||
      ||
      ||
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||  /
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||  /|
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||  /|\
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||   |
      ||   O
      ||  /|\
      ||  / \
      ||
      |""".stripMargin
  )
}

trait IODevice {
  def printLine(text: String): Unit

  def readLine(): String
}

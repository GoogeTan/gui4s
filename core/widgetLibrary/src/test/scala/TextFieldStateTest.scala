import gui4s.core.widget.library.{TextFieldState, TextPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

final class TextFieldStateTest extends AnyFlatSpec with should.Matchers:
  val exampleText = "Прозвучало над ясной рекою,\nПрозвенело в померкшем лугу,\nПрокатилось над рощей немою,\nЗасветилось на том берегу.\n\nДалеко, в полумраке, луками\nУбегает на запад река.\nПогорев золотыми каймами,\nРазлетелись, как дым, облака.\n\nНа пригорке то сыро, то жарко,\nВздохи дня есть в дыханье ночном,-\nНо зарница уж теплится ярко\nГолубым и зелёным огнём."
  val exampleTextLines: List[String] = exampleText.split("\n", -1).toList

  "cursorStartIndex" should "satisfy examples" in:
    assert(new TextFieldState().cursorStartIndex == 0)
    assert(new TextFieldState("123\n").cursorStartIndex == 0)
    assert(new TextFieldState("123\n12414", TextPosition(1, 0)).cursorStartIndex == 4)
    assert(new TextFieldState("123\n12414", TextPosition(1, 1)).cursorStartIndex == 5)
    assert(new TextFieldState("123\n12414", TextPosition(1, 5)).cursorStartIndex == 9)

  "Text insertion" should "remove selected text" in:
    new TextFieldState("123\n12414", TextPosition(1, 1), TextPosition(1, 5))
      .insert("5678") shouldBe new TextFieldState("123\n15678", TextPosition(1, 5))

    new TextFieldState("123\n12414", TextPosition(1, 1), TextPosition(1, 5))
      .insert(" ") shouldBe new TextFieldState("123\n1 ", TextPosition(1, 2))

    new TextFieldState("123\n12414", TextPosition(1, 1), TextPosition(1, 5))
      .insert("\n") shouldBe new TextFieldState("123\n1\n", TextPosition(2, 0))

    new TextFieldState("123\n12414", TextPosition(1, 1), TextPosition(1, 5))
      .insert("\n1") shouldBe new TextFieldState("123\n1\n1", TextPosition(2, 1))

  "backspace" should "remove selected text" in:
    new TextFieldState("123\n12414", TextPosition(1, 1), TextPosition(1, 5))
      .backspace() shouldBe new TextFieldState("123\n1", TextPosition(1, 1))

  "backspace" should "remove character before cursor" in:
    new TextFieldState("123\n12414", TextPosition(1, 1))
      .backspace() shouldBe new TextFieldState("123\n2414", TextPosition(1, 0))

    new TextFieldState("123\n", TextPosition(1, 0))
      .backspace() shouldBe new TextFieldState("123", TextPosition(0, 3))

    new TextFieldState("123\n", TextPosition(1, 0))
      .backspace() shouldBe new TextFieldState("123", TextPosition(0, 3))

  "backspace" should "do nothing if cursor is at the beginning" in:
      new TextFieldState("123\n12414", TextPosition(0, 0))
        .backspace() shouldBe new TextFieldState("123\n12414", TextPosition(0, 0))

  "backspace" should "work with vertical movement" in:
      new TextFieldState("123\n12414", TextPosition(1, 50))
        .backspace() shouldBe new TextFieldState("123\n1241", TextPosition(1, 4))

  "move up" should "work with empty lines" in:
    new TextFieldState("\n\n12414", TextPosition(2, 0))
      .moveCursorUp(false) shouldBe new TextFieldState("\n\n12414", TextPosition(1, 0))

    new TextFieldState("\n\n12414", TextPosition(2, 7))
      .moveCursorUp(false) shouldBe new TextFieldState("\n\n12414", TextPosition(1, 0))

  "move up" should "keep horizontal position" in:
    new TextFieldState("123\n\n12414", TextPosition(1, 5))
      .moveCursorUp(false) shouldBe new TextFieldState("123\n\n12414", TextPosition(0, 5))
end TextFieldStateTest

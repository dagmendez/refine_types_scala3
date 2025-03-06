package dagmendez.libraries

import scala.annotation.tailrec
import dagmendez.libraries.common.*
import neotype.*

/**
 * This is an example of how to implement a basic validator for Spanish IDs, for nationals and foreign residents. Find the information on how the IDs
 * are validated - in Spanish -
 * [[https://www.interior.gob.es/opencms/es/servicios-al-ciudadano/tramites-y-gestiones/dni/calculo-del-digito-de-control-del-nif-nie/ here]]
 */
object NeoType:

  type ValidNumber = ValidNumber.Type
  object ValidNumber extends Newtype[String]:
    override inline def validate(input: String): Boolean | String =
      input match
        case nan if nan.toIntOption.isEmpty    => InvalidNumber(input).toString
        case negative if negative.toInt < 0    => InvalidNegativeNumber(negative).toString
        case tooBig if tooBig.toInt > 99999999 => InvalidTooBigNumber(tooBig).toString
        case _                                 => true

  type DNI = DNI.Type
  object DNI extends Newtype[(ValidNumber, ControlLetter)]:
    override inline def validate(input: (ValidNumber, ControlLetter)): Boolean | String =
      val number = input._1.unwrap
      val letter = input._2
      if letter.isValidId(number.toInt) then true
      else InvalidId(number).toString

  type NIE = NIE.type
  object NIE extends Newtype[(NieLetter, ValidNumber, ControlLetter)]:
    override def validate(input: (NieLetter, ValidNumber, ControlLetter)): Boolean | String =
      val nieLetter     = input._1
      val number        = input._2.unwrap
      val letter        = input._3
      val composeNumber = s"${nieLetter.ordinal}$number".toInt
      if letter.isValidId(composeNumber) then true
      else InvalidId(s"$nieLetter-$number").toString

  type ID = ID.type
  object ID extends Newtype[String]:
    override def validate(input: String): Boolean | String =
      if input.length > 9
      then InvalidIdTooLong(input).toString
      else
        val (number, letter) = input.splitAt(8)
        val isDni            = number.head.isDigit
        val result =
          if isDni
          then
            for
              n <- ValidNumber.make(number)
              l <- ControlLetter.parse(letter).swap.map(_.toString).swap
            yield DNI.make(n, l)
          else
            for
              nl <- NieLetter.parse(number.head.toString).swap.map(_.toString).swap
              n  <- ValidNumber.make(number.tail)
              l  <- ControlLetter.parse(letter).swap.map(_.toString).swap
            yield NIE.make(nl, n, l)
        result.flatMap(either => either.map(_ => true)) match
          case Left(error) => error
          case Right(_)    => true

  @main
  def validate(): Unit =

    println("""
        | *----------------------*
        | | Spanish ID validator |
        | *----------------------*
        |
        | Introduce any ID. For example:
        | - 12345678-Z
        | - 12345678-z
        | - 12345678Z
        | - 12345678z
        | - Y-2345678-Z
        | - Y2345678Z
        | - y-2345678-z
        | - y2345678z
        |
        | write QUIT, quit, Q or q to exit the program
        |
        |""".stripMargin)
    @tailrec
    def loop(): Unit =
      val userInput   = scala.io.StdIn.readLine("Enter a DNI: ")
      val exitCommand = Set("QUIT", "Q", "quit", "q")
      if exitCommand.contains(userInput) then ()
      else {
        ID.make(userInput.trim.replace("-", "").toUpperCase) match
          case Right(id)   => println(s"$id is a valid ID")
          case Left(error) => println(s"$userInput is not a valid ID. Reason: $error")
        loop()
      }
    loop()

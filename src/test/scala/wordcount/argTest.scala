package hellotest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.*

class HandleArgsSpec extends AnyFlatSpec:

  "handleArgs" should "return default values when no arguments are passed" in:
    val handler = new HandleArgs
    val result = handler.handleArgs(Array())
    result must equal(List(
      "cloud-size: 10",
      "length-at-least: 6",
      "window-size: 1000"
    ))

  it should "return correct values for short options" in:
    val handler = new HandleArgs
    val result = handler.handleArgs(Array("-c", "3", "-l", "2", "-w", "5"))
    result must equal(List(
      "cloud-size: 3",
      "length-at-least: 2",
      "window-size: 5"
    ))

  it should "return correct values for long options" in:
    val handler = new HandleArgs
    val result = handler.handleArgs(Array("--cloud-size", "3", "--length-at-least", "2", "--window-size", "5"))
    result must equal(List(
      "cloud-size: 3",
      "length-at-least: 2",
      "window-size: 5"
    ))

  it should "correctly handle mixed short and long options" in:
    val handler = new HandleArgs
    val result = handler.handleArgs(Array("-c", "3", "--length-at-least", "2", "-w", "5"))
    result must equal(List(
      "cloud-size: 3",
      "length-at-least: 2",
      "window-size: 5"
    ))

  it should "return default values for missing options" in:
    val handler = new HandleArgs
    val result = handler.handleArgs(Array("-c", "4"))
    result must equal(List(
      "cloud-size: 4",
      "length-at-least: 6",
      "window-size: 1000"
    ))

end HandleArgsSpec

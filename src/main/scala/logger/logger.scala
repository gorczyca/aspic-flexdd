package logger


import java.time.LocalTime
import java.time.format.DateTimeFormatter


def log(on: Boolean)(str: String): Unit = {
  if (on) {
    val currentTime = LocalTime.now()
    val formatter = DateTimeFormatter.ofPattern("HH:mm:ss")

    val formattedTime = currentTime.format(formatter)
    println(s"[$formattedTime] $str")
  }
}



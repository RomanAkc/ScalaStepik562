import scala.math._

def isZero(x: Int):  Either[String, Int] = {
  if(x == 0) Left("Zero divisor")
  else Right(x)
}

def isDelimiterZero(p: (Int, Int)): Either[String, (Int, Int)] = {
  isZero(p._2).flatMap(x => Right((p._1, x)))
}

def isRightNUmber(p: (Int, Int)) : Either[String, (Int, Int)] = {
  isDelimiterZero(p).flatMap { x =>
    if(scala.math.abs(x._1) > scala.math.abs(x._2)) Left("Invalid input")
    else Right(x)
  }
}

def rawDivide(p: (Int, Int), q: (Int, Int)) : Either[String, (Int, Int)] = {
  isRightNUmber(p).flatMap { sp =>
    isRightNUmber(q).flatMap { sq =>
      isZero(q._1).flatMap{ sq1 =>
        Right((sp._1 * sq._2), (sp._2 * sq1))
      }
    }
  }
}

def divide(p: (Int, Int))(q: (Int, Int)): Either[String, (Int, Int)] = {
  rawDivide(p, q).flatMap{ r =>
    if(isRightNUmber(r) == Left("Invalid input")) Left("Improper result")
    else {
      val gc = scala.math.BigInt(r._1).gcd(scala.math.BigInt(r._2))
      Right((r._1/gc.toInt, r._2/gc.toInt))
    }
  }
}


divide(3,5)(4,5)
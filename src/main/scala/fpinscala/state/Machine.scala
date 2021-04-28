package fpinscala.state

import State._
import Machine._
import shapeless.tag
import shapeless.tag._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Candies, coins: Coins)

object Machine {
  type Candies = Int @@ CandyTag
  type Coins = Int @@ CoinTag

  val tagCandies: Int => Candies = tag[CandyTag][Int]
  val tagCoins: Int => Coins = tag[CoinTag][Int]

  def simulateMachine(inputs: List[Input]): State[Machine, (Coins, Candies)] = {
    val states: List[State[Machine, Unit]] = inputs.map { i =>
      modify[Machine] { m =>
        (i, m) match {
          case (Coin, Machine(true, candies, coins)) if candies > 0 =>
            Machine(locked = false, candies, tagCoins(coins + 1))
          case (Turn, Machine(false, candies, coins)) =>
            Machine(locked = true, tagCandies(candies - 1), coins)
          case _ => m
        }
      }
    }

    for {
      _ <- sequence(states)
      Machine(_, candies, coins) <- get
    } yield (coins, candies)
  }
}

trait CandyTag
trait CoinTag

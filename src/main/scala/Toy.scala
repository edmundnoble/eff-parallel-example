import cats.implicits._
import org.atnos.eff.{Eff, |=}
import org.atnos.eff.all._
import org.atnos.eff.future._

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

sealed trait Toy[A]
final case class Getting(key: String)                extends Toy[Option[String]]
final case class Setting(key: String, value: String) extends Toy[Unit]
final case class Doing(v: String)                    extends Toy[String]

object Toy {
  type _toy[R] = Toy |= R

  def get[R  : _toy](key: String): Eff[R, Option[String]] =
    Eff.send(Getting(key))

  def set[R  : _toy](k: String, v: String): Eff[R, Unit] =
    Eff.send(Setting(k, v))

  def doing[R  : _toy](v: String): Eff[R, String] =
    Eff.send(Doing(v))

  def cycle[R  : _toy](x: String): Eff[R, String] = for {
    v <- get(x)
    s <- doing(v.getOrElse("N/A"))
  } yield s

  def groupCycle[R : _toy](xs: List[String]): Eff[R, List[String]] =
    xs.traverse(cycle[R])

  /** NEW */
  def parGroupCycle[R : _toy](xs: List[String])(implicit ec: ExecutionContext): Future[Eff[R, List[String]]] = {
    def futureCycle(x: String): Future[Eff[R, String]] = {
      val prom = Promise[Eff[R, String]]
      Future(cycle(x)).onComplete {
        case Success(a) => prom.success(a)
        case Failure(e) => prom.success(Eff.pure(s"Cycle failed: ${e.getMessage}"))
      }
      prom.future
    }

    Future.traverse(xs)(futureCycle).map(Eff.sequenceA(_))
  }
}
 

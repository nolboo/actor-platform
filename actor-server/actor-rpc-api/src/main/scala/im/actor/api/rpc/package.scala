package im.actor.api

import cats.data.XorT
import cats.data.Xor

import scala.concurrent.{ ExecutionContext, Future }
import scala.language.implicitConversions
import scala.reflect._
import scalaz._

import slick.dbio.{ DBIO, DBIOAction }
import slick.driver.PostgresDriver.api._

package object rpc extends {

  import slick.dbio.NoStream

  object Implicits extends PeersImplicits with GroupsImplicits with HistoryImplicits

  object CommonErrors {
    val GroupNotFound = RpcError(404, "GROUP_NOT_FOUND", "", false, None)
    val InvalidAccessHash = RpcError(403, "INVALID_ACCESS_HASH", "", false, None)
    val UnsupportedRequest = RpcError(400, "REQUEST_NOT_SUPPORTED", "Operation not supported.", false, None)
    val UserNotAuthorized = RpcError(403, "USER_NOT_AUTHORIZED", "", false, None)
    val UserNotFound = RpcError(404, "USER_NOT_FOUND", "", false, None)
    val UserPhoneNotFound = RpcError(404, "USER_PHONE_NOT_FOUND", "", false, None)

    def forbidden(userMessage: String) = RpcError(403, "FORBIDDEN", userMessage, false, None)
  }

  type OkResp[+A] = A

  object Error {
    def apply[A](e: RpcError)(implicit ev: A <:< RpcResponse): RpcError \/ A =
      -\/(e)

    def unapply(v: RpcError \/ _) =
      v match {
        case -\/(e) ⇒ Some(e)
        case _      ⇒ None
      }
  }

  object Ok {
    def apply[A](rsp: A)(implicit ev: A <:< RpcResponse): RpcError \/ A =
      \/-(rsp)

    def unapply[T <: OkResp[RpcResponse]](v: _ \/ T)(implicit m: ClassTag[T]) =
      v match {
        case \/-(t) ⇒ Some(t)
        case -\/(_) ⇒ None
      }
  }

  def authorizedAction[R](clientData: ClientData)(f: AuthorizedClientData ⇒ DBIOAction[RpcError \/ R, NoStream, Nothing])(implicit db: Database): Future[RpcError \/ R] = {
    val authorizedAction = requireAuth(clientData).map(f)
    db.run(toDBIOAction(authorizedAction))
  }

  def requireAuth(implicit clientData: ClientData): MaybeAuthorized[AuthorizedClientData] =
    clientData.optUserId match {
      case Some(userId) ⇒ Authorized(AuthorizedClientData(clientData.authId, clientData.sessionId, userId))
      case None         ⇒ NotAuthorized
    }

  def toDBIOAction[R](
    authorizedAction: MaybeAuthorized[DBIOAction[RpcError \/ R, NoStream, Nothing]]
  ): DBIOAction[RpcError \/ R, NoStream, Nothing] =
    authorizedAction.getOrElse(DBIO.successful(-\/(RpcError(403, "USER_NOT_AUTHORIZED", "", false, None))))

  def authorizedClient(clientData: ClientData): Result[AuthorizedClientData] =
    DBIOResult.fromOption(CommonErrors.UserNotFound)(clientData.optUserId.map(id ⇒ AuthorizedClientData(clientData.authId, clientData.sessionId, id)))

  type Result[A] = XorT[DBIO, RpcError, A]

  def dbioXorToEither[A, B](v: DBIO[A Xor B])(implicit ec: ExecutionContext): DBIO[A \/ B] = v map {
    case Xor.Left(l) => -\/(l)
    case Xor.Right(r) => \/-(r)
  }

  object DBIOResult {
    implicit def dbioFunctor(implicit ec: ExecutionContext): cats.Functor[DBIO] = new cats.Functor[DBIO] {
      def map[A, B](fa: DBIO[A])(f: (A) => B) = fa map f
    }
    implicit def dbioMonad(implicit ec: ExecutionContext): cats.Monad[DBIO] = new cats.Monad[DBIO] {
      def pure[A](x: A) = DBIO.successful(x)
      def flatMap[A, B](fa: DBIO[A])(f: (A) => DBIO[B]) = fa flatMap f
    }

    implicit class Opt2Xor[A](val self: Option[A]) {
      def toXor[B](failure: B): B Xor A = self map Xor.right getOrElse Xor.left(failure)
    }

    implicit class Boolean2Xor(val self: Boolean) {
      def toXor[A](failure:A): A Xor Unit = if (self) Xor.right(()) else Xor.left(failure)
    }

    def point[A](a: A): Result[A] = XorT[DBIO, RpcError, A](DBIO.successful(Xor.right(a)))

    def fromDBIO[A](fa: DBIO[A])(implicit ec: ExecutionContext): Result[A] = XorT[DBIO, RpcError, A](fa map Xor.right)

    def fromEither[A](va: RpcError Xor A): Result[A] = XorT[DBIO, RpcError, A](DBIO.successful(va))

    def fromEither[A, B](failure: B ⇒ RpcError)(va: B Xor A): Result[A] = XorT[DBIO, RpcError, A](DBIO.successful(va leftMap failure))

    def fromOption[A](failure: RpcError)(oa: Option[A]): Result[A] = XorT[DBIO, RpcError, A](DBIO.successful(oa.toXor(failure)))

    def fromDBIOOption[A](failure: RpcError)(foa: DBIO[Option[A]])(implicit ec: ExecutionContext): Result[A] =
      XorT[DBIO, RpcError, A](foa map(_.toXor(failure)))

    def fromDBIOBoolean(failure: RpcError)(foa: DBIO[Boolean])(implicit ec: ExecutionContext): Result[Unit] =
      XorT[DBIO, RpcError, Unit](foa map(_.toXor(failure)))

    def fromDBIOEither[A, B](failure: B ⇒ RpcError)(fva: DBIO[B Xor A])(implicit ec: ExecutionContext): Result[A] =
      XorT[DBIO, RpcError, A](fva map(_.leftMap(failure)))

    def fromFuture[A](fu: Future[A])(implicit ec: ExecutionContext): Result[A] = XorT[DBIO, RpcError, A](DBIO.from(fu map Xor.right))

    def fromFutureOption[A](failure: RpcError)(fu: Future[Option[A]])(implicit ec: ExecutionContext): Result[A] = XorT[DBIO, RpcError, A](DBIO.from(fu map(_.toXor(failure))))

    def fromBoolean(failure: RpcError)(oa: Boolean): Result[Unit] = XorT[DBIO, RpcError, Unit](DBIO.successful(oa.toXor(failure)))
  }

}

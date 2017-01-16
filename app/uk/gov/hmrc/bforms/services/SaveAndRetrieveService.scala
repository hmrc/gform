package uk.gov.hmrc.bforms.services

import uk.gov.hmrc.bforms.repositories.SaveAndRetrieveRepository

import scala.concurrent.Future

trait Save[A] {
  def apply(a:A):Future[Either[String, Unit]]
}

object Save {

  private def getTaxFormSave[A](f:A => Future[Either[String, Unit]]) : Save[A] = {
    new Save[A] {
      override def apply(params: A): Future[Either[String, Unit]] = f(params)
    }
  }

  implicit def saveData(implicit repository: SaveAndRetrieveRepository) : Save[Map[String, String]] = {
    getTaxFormSave((r : Map[String, String]) => repository.save(r))
  }

}

object SaveService {

  def save[A](formData: A)(implicit save : Save[A]) : Future[Either[String, Unit]] = {
    save(formData).map{
      case Left(x) => Left(x)
      case Right(()) => Right(())
    }
  }

}

trait Retrieve[A, B] {
  def apply(a:A): Future[List[B]]
}

object Retrieve {

  private def retrieveData[A, B](f:A => Future[List[B]]) : Retrieve[A, B] = {
    new Retrieve[A, B] {
      def apply(params : A) : Future[List[B]] = f(params)
    }
  }

  implicit def retrieveFormData(implicit repository: SaveAndRetrieveRepository) = {
    retrieveData((f: String) => repository.retrieve(f))
  }

}

object RetrieveService {

  def retrieve[A, B <: Map[String, String]](registrationNumber: A)(implicit retrieve: Retrieve[A ,B]): Future[Either[Map[String, String], Unit]] = {
    retrieve(registrationNumber).flatMap{
      case obj : List[Map[String, String]] if obj.isEmpty => Future.successful(Right(()))
      case obj  => Future.successful(Left(obj(0)))
      case _ => Future.successful(Right(()))
    }
  }

}
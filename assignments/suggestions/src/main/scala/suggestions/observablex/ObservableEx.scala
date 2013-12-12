package suggestions
package observablex

import scala.concurrent.{Future, ExecutionContext}
import scala.util._
import scala.util.Success
import scala.util.Failure
import java.lang.Throwable
import rx.lang.scala.Observable
import rx.lang.scala.Observer
import rx.lang.scala.Scheduler
import rx.lang.scala.subjects.ReplaySubject
import rx.lang.scala.subscriptions.BooleanSubscription

object ObservableEx {

  /** Returns an observable stream of values produced by the given future.
   * If the future fails, the observable will fail as well.
   *
   * @param f future whose values end up in the resulting observable
   * @return an observable completed after producing the value of the future, or with an exception
   */
  def apply[T](f: Future[T])(implicit execContext: ExecutionContext): Observable[T] = Observable({observer: Observer[T] => 
    
      val rs = ReplaySubject[T]()
      f.onSuccess {
        case t => rs.onNext(t); rs.onCompleted()
      }
      f.onFailure {
        case f => rs.onError(f)
      }
      
      BooleanSubscription {rs.onError(new Exception("cancelled by user"))}
    })
}
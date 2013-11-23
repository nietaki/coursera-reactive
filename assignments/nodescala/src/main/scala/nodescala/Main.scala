package nodescala

import scala.language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}
//import nodescala.FutureCompanionOps

object Main {

  def main(args: Array[String]) {
    // TO IMPLEMENT
    // 1. instantiate the server at 8191, relative path "/test",
    //    and have the response return headers of the request
    val subscriptionPromise = Promise[Subscription]
    val myServer = new NodeScala.Default(8191)
    lazy val myChatSubscription = myServer.start("/chat")(chatHandler)
    subscriptionPromise.success(myChatSubscription)
    lazy val chatHandler: nodescala.NodeScala.Request => nodescala.NodeScala.Response = req => { 
      val res = Await.result(operatorResponse, 10 minutes)
      if (res == "quit") {
        subscriptionPromise.future.onSuccess(_ match {case s: Subscription => s.unsubscribe()})
      }
      List(res).iterator
    }
    def operatorResponse: Future[String] = {
      val userMessage = Future.userInput("please respond to chat message: \n")
      val timeoutMessage = messageAfterTimeout("chat operator timeout!", 10 seconds)
      Future.any(List(userMessage, timeoutMessage))
    }
    def messageAfterTimeout(message: String, timeout: Duration): Future[String] = {
      val p = Promise[String]
      val f = Future.delay(timeout)
      f.onComplete(sth => p.success(message))
      p.future
    }
  }

}
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
    val myServer = new NodeScala.Default(8191)
    val myServerSubscription = myServer.start("/test")(req => "This is a sample response".split(" ").iterator)

    // TO IMPLEMENT
    // 2. create a future that expects some user input `x`
    //    and continues with a `"You entered... " + x` message
    val userInterrupted: Future[String] = Future.userInput("please respond") 
    // TO IMPLEMENT
    // 3. create a future that completes after 20 seconds
    //    and continues with a `"Server timeout!"` message
    
    def messageAfterTimeout(message: String, timeout: Duration): Future[String] = {
      val p = Promise[String]
      val f = Future.delay(timeout)
      f.onComplete(sth => p.success(message))
      p.future
    }
    
    val timeOut: Future[String] = messageAfterTimeout("Server timeout!", 20 seconds)
    // TO IMPLEMENT
    // 4. create a future that completes when either 10 seconds elapse
    //    or the user enters some text and presses ENTER
    val terminationRequested: Future[String] = 
      Future.any(timeOut :: userInterrupted :: Nil) 

    // TO IMPLEMENT
    // 5. unsubscribe from the server
    terminationRequested onSuccess {
      case msg => {
        myServerSubscription.unsubscribe()
        println("bye")
      }
    }
  }

}
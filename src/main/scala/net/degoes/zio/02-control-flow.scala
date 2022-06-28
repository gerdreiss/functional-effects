package net.degoes.zio

import zio._

object Looping extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Implement a `repeat` combinator using `flatMap` (or `zipRight`) and recursion.
   */
  def repeat[R, E, A](n: Int)(effect: ZIO[R, E, A]): ZIO[R, E, Chunk[A]] =
    if (n <= 0) ZIO.succeed(Chunk.empty)
    else effect.zipWith(repeat(n - 1)(effect))(_ +: _)
  //for {
  //  a  <- effect
  //  as <- repeat(n - 1)(effect)
  //} yield a +: as

  def repeatCollect[R, E, A](n: Int)(effect: ZIO[R, E, A]): ZIO[R, Nothing, Chunk[Either[E, A]]] =
    repeat(n)(effect.either)

  val run =
    repeatCollect(100)(Console.printLine("All work and no play makes Jack a dull boy"))
}

object Interview extends ZIOAppDefault {
  import java.io.IOException

  val questions =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  def getAnswer(question: String): IO[IOException, String] =
    Console.readLine(question)
      .flatMap(answer =>
        if (answer.isEmpty)
          getAnswer(question)
        else
          ZIO.succeed(answer))

  /**
   * EXERCISE
   *
   * Implement the `getAllAnswers` function in such a fashion that it will ask
   * the user each question and collect them all into a list.
   */
  def getAllAnswers(questions: List[String]): ZIO[Any, IOException, List[String]] =
    questions match {
      case Nil     => ZIO.succeed(Nil)
      case q :: qs => Console.readLine(s"$q> ").zipWith(getAllAnswers(qs))(_ :: _)
    }

  /**
   * EXERCISE
   *
   * Use the preceding `getAllAnswers` function, together with the predefined
   * `questions`, to ask the user a bunch of questions, and print the answers.
   */
  val run =
    getAllAnswers(questions)
      .flatMap(answers =>
        Console.printLine(answers.mkString("\n")))
}

object InterviewGeneric extends ZIOAppDefault {

  val questions =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Implement the `iterateAndCollect` function.
   */
  def iterateAndCollect[R, E, A, B](as: List[A])(f: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
    as match {
      case Nil     => ZIO.succeed(Nil)
      case a :: as => f(a).zipWith(iterateAndCollect(as)(f))(_ :: _)
    }

  val run =
    iterateAndCollect(questions) { question =>
      Console.printLine(question) *>
        Console.readLine("> ")
    }
}

object InterviewForeach extends ZIOAppDefault {

  val questions =
    "Where where you born?" ::
      "What color are your eyes?" ::
      "What is your favorite movie?" ::
      "What is your favorite number?" :: Nil

  /**
   * EXERCISE
   *
   * Using `ZIO.foreach`, iterate over each question in `questions`, print the
   * question to the user (`Console.printLine`), read the answer from the user
   * (`Console.readLine`), and collect all answers into a collection. Finally, print
   * out the contents of the collection.
   */
  val run =
    ZIO.foreach(questions) { question =>
      Console.printLine(question) *>
        Console.readLine("> ")
    }
}

object WhileLoop extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Implement the functional effect version of a while loop so the
   * application runs correctly.
   */
  def whileLoop[R, E, A](cond: UIO[Boolean])(zio: ZIO[R, E, A]): ZIO[R, E, Chunk[A]] = {
    //cond.flatMap {
    //  case true => zio.flatMap(a => whileLoop(cond)(zio).map(a +: _))
    //  case false => ZIO.succeed(Chunk.empty)
    //}
    for {
      continue <- cond
      chunk <- if (continue) zio.zipWith(whileLoop(cond)(zio))(_ +: _)
              else ZIO.succeed(Chunk.empty)
    } yield chunk
  }

  val run = {
    def loop(variable: Ref[Int]) =
      whileLoop(variable.get.map(_ < 100)) {
        for {
          value <- variable.getAndUpdate(_ + 1)
          _     <- Console.printLine(s"At iteration: $value")
        } yield ()
      }

    (for {
      variable <- Ref.make(0)
      _        <- loop(variable)
    } yield 0)
  }
}

object Iterate extends ZIOAppDefault {

  /**
   * EXERCISE
   *
   * Implement the `iterate` function such that it iterates until the condition
   * evaluates to false, returning the "last" value of type `A`.
   */
  def iterate[R, E, A](start: A)(cond: A => Boolean)(f: A => ZIO[R, E, A]): ZIO[R, E, A] =
    if (cond(start)) f(start).flatMap(iterate(_)(cond)(f))
    else ZIO.succeed(start)

  val run =
    iterate(0)(_ < 100) { i =>
      Console.printLine(s"At iteration: ${i}").as(i + 1)
    }
}

object TailRecursive extends ZIOAppDefault {
  trait Response
  trait Request {
    def returnResponse(response: Response): Task[Unit]
  }

  lazy val acceptRequest: Task[Request] = ZIO.attempt(new Request {
    def returnResponse(response: Response): Task[Unit] =
      ZIO.attempt(println(s"Returning response $response"))
  })

  def handleRequest(request: Request): Task[Response] = ZIO.attempt {
    println(s"Handling request $request")
    new Response {}
  }

  /**
   * EXERCISE
   *
   * Make this infinite loop (which represents a webserver) effectfully tail
   * recursive.
   */
  lazy val webserver: Task[Nothing] =
    (for {
      request  <- acceptRequest
      response <- handleRequest(request)
      _        <- request.returnResponse(response)
      _  <- webserver
    } yield ()).forever

  val run =
    for {
      fiber <- webserver.fork
      _     <- ZIO.sleep(100.millis)
      _     <- fiber.interrupt
    } yield ()
}

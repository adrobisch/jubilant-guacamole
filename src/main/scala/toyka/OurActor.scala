package toyka

import scala.collection.mutable

trait OurActor {
  def receive(msg: Any, sender: OurActor)
  def !(message: Message, receiver: OurActor): Unit = receiver.receive(message, this)

  protected def createActor(clazz: Class[_]): OurActor = clazz.newInstance().asInstanceOf[OurActor]
}

trait Message
case object RequestBatch extends Message
case class Batch(words: Seq[String]) extends Message
case class ParserUrl(url: String) extends Message

class Parser(counter: OurActor) extends OurActor {
  override def receive(msg: Any, sender: OurActor): Unit = msg match {
    case ParserUrl(url) => counter.receive(Batch(parse(url)), this)
    // case RequestBatch => this.!(Batch(), sender)
  }

  def parse(url: String): Seq[String] = Seq("foo", "bar", "foo")
}

class Counter extends OurActor {
  val counts = new mutable.HashMap[String, Int]()

  override def receive(msg: Any, sender: OurActor): Unit = msg match {
    case Batch(words) => words.groupBy(word => word).foreach {
      case (word, occurrences) => counts.update(word, counts.getOrElse(word, 0) + occurrences.length)
    }
  }
}

object OurActorApp extends App {
  val counter = new Counter()
  val parser = new Parser(counter)

  parser.receive(ParserUrl("a url"), null)
  println(counter.counts)
}

package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, composable operators allow building infinitely many
 * solutions from a few operators and domain constructors.
 *
 * Operators and constructors are either primitive, meaning they cannot be
 * expressed in terms of others, or they are derived, meaning they can be
 * expressed in terms of other operators or constructors.
 *
 * The choice of primitives determine how powerful and expressive a domain
 * model is. Some choices lead to weaker models, and others, to more powerful
 * models. Power is not always a good thing: constraining the power of a model
 * allows more efficient and more feature-full execution.
 *
 * Derived operators and constructors bridge the gap from the domain, to common
 * problems that a user of the domain has to solve, improving productivity.
 *
 * In many domains, there exist many potential choices for the set of primitive
 * operators and constructors. But not all choices are equally good.
 *
 * The best primitives are:
 *
 * * Composable, to permit a lot of power in a small, reasonable package
 * * Expressive, to solve the full range of problems in the domain
 * * Orthogonal, such that no primitive provides the capabilities of any other
 *
 * Orthogonality also implies minimalism, which means the primitives are the
 * smallest set of orthogonal primitives that exist.
 *
 */

/**
 * ORTHOGONALITY - EXERCISE SET 1
 */
object email_filter3 {
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  /**
   * EXERCISE 1
   *
   * In the following model, which describes an email filter, there are many
   * primitives with overlapping responsibilities. Find the smallest possible
   * set of primitive operators and constructors, without deleting any
   * constructors or operators (you may implement them in terms of primitives).
   *
   * NOTE: You may *not* use a final encoding, which would allow you to
   * collapse everything down to one primitive.
   */
  sealed trait EmailFilter { self =>
    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    def ||(that: EmailFilter): EmailFilter = !(!self && !that)

    def ^^(that: EmailFilter): EmailFilter = (self || that) && !(self && that)

    def unary_! : EmailFilter = EmailFilter.Not(self)
  }

  object EmailFilter {
    final case object Always                                    extends EmailFilter
    final case object Never                                     extends EmailFilter
    final case class And(left: EmailFilter, right: EmailFilter) extends EmailFilter
    final case class Not(value: EmailFilter)                    extends EmailFilter
    final case class SenderEquals(target: Address)              extends EmailFilter
    final case class RecipientEquals(target: Address)           extends EmailFilter
    final case class BodyContains(phrase: String)               extends EmailFilter
    final case class SubjectContains(phrase: String)            extends EmailFilter

    val always: EmailFilter = Always

    val never: EmailFilter = Never

    def senderIs(sender: Address): EmailFilter = SenderEquals(sender)

    def senderIsNot(sender: Address): EmailFilter = !senderIs(sender)

    def recipientIs(recipient: Address): EmailFilter = RecipientEquals(recipient)

    def recipientIsNot(recipient: Address): EmailFilter = !recipientIs(recipient)

    def senderIn(senders: Set[Address]): EmailFilter =
      senders.foldLeft(never)((acc, address) => acc || SenderEquals(address))

    def recipientIn(recipients: Set[Address]): EmailFilter =
      recipients.foldLeft(never)((acc, address) => acc || RecipientEquals(address))

    def bodyContains(phrase: String): EmailFilter = BodyContains(phrase)

    def bodyDoesNotContain(phrase: String): EmailFilter = !bodyContains(phrase)

    def subjectContains(phrase: String): EmailFilter = SubjectContains(phrase)

    def subjectDoesNotContain(phrase: String): EmailFilter = !subjectContains(phrase)
  }
}

/**
 * COMPOSABILITY - EXERCISE SET 2
 */
object ui_components {

  /**
   * EXERCISE 1
   *
   * The following API is not composable—there is no domain. Introduce a
   * domain with elements, constructors, and composable operators.
   */
  trait Turtle { self =>
    def turnLeft(degrees: Int): Unit

    def turnRight(degrees: Int): Unit

    def goForward(): Unit

    def goBackward(): Unit

    def draw(): Unit
  }

  // Assuming angular coordinates
  //
  object declarative {

    sealed trait Movement { self =>

      def andThen(that: Movement): Movement = Movement.AndThen(self, that)
    }

    object Movement {

      final case class AndThen(fst: Movement, snd: Movement) extends Movement
      case object Forward                                    extends Movement
      case object TurnLeft                                   extends Movement

      def backward: Movement = turnLeft(180) andThen Forward andThen turnRight(180)

      def backward(n: Int): Movement = (1 to n).foldLeft(nothing)((path, _) => backward andThen path)

      def forward: Movement = Forward

      def forward(n: Int): Movement = (1 to n).foldLeft(nothing)((path, _) => TurnLeft andThen TurnLeft)

      def turnLeft(degrees: Int): Movement = (1 to degrees).foldLeft(nothing)((path, _) => Forward andThen path)

      def turnRight(degrees: Int): Movement = turnLeft(360 - degrees)

      def nothing: Movement = turnLeft(360)
    }

    final case class Position(radius: Int, angle: Int) { self =>
      def incAngle(increment: Int): Position = Position(radius, angle + increment)

      def incRadius(increment: Int): Position = Position(radius + increment, angle)
    }

    def interpret(start: Position, movement: Movement): List[Position] = {
      import Movement._
      def loop(head: Position, tail: List[Position], pending: Movement): List[Position] = pending match {
        case AndThen(fst, snd) =>
          val hd :: tl = loop(head, tail, fst)
          loop(hd, tl, snd)
        case Forward  => head.incRadius(1) :: tail
        case TurnLeft => head.incAngle(1) :: tail
      }

      loop(start, Nil, movement).foldLeft(List.empty[Position]) {
        case (lst @ (hd :: _), pos) if hd == pos => lst
        case (lst, pos)                          => pos :: lst
      }

      // Unoptimised alternative:
      //
      //loop(start, Nil, movement).reverse
    }

  }
}

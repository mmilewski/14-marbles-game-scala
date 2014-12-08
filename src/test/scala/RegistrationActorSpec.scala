import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import core.RegistrationActor.{NotRegistered, Register, Registered}
import core.{Core, CoreActors}
import org.specs2.mutable.SpecificationLike

class RegistrationActorSpec
  extends TestKit(ActorSystem())
  with SpecificationLike
  with CoreActors with Core
  with ImplicitSender {

  sequential

  "Registration " should {

    "reject invalid registration request" in {
      registration ! Register("marcin")
      expectMsgPF() {
        case Left(NotRegistered(reason, event)) =>
          reason mustEqual "Name is already taken"
          event mustEqual "notRegistered"
      }
      success
    }

    "accept valid registration request" in {
      registration ! Register("Marcin")
      expectMsgPF() {
        case Right(Registered(uid, event)) => event mustEqual "registered"
      }
      success
    }

  }
}


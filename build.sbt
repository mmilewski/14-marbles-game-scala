name := "14-marbles-game-scala"

version := "1.0"

scalaVersion := "2.11.4"

scalacOptions in Test ++= Seq(
  "-Yrangepos"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-Ywarn-dead-code",
//  "-language:_",
//  "-target:jvm-1.7",
  "-encoding", "UTF-8"
)

javaOptions in Test ++= Seq(
  "-Xmx2048m",
  "-XX:+CMSClassUnloadingEnabled"
)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= {
  val akkaV = "2.3.7"
  val sprayV = "1.3.2"
  Seq(
    "io.spray" %% "spray-can" % sprayV,
    "io.spray" %% "spray-io" % sprayV,
    "io.spray" %% "spray-http" % sprayV,
    "io.spray" %% "spray-httpx" % sprayV,
    "io.spray" %% "spray-json" % "1.3.1",
    "io.spray" %% "spray-routing" % sprayV,
    "io.spray" %% "spray-testkit" % sprayV % "test",
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-testkit" % akkaV % "test",
    "org.specs2" %% "specs2-core" % "2.4.13" % "test"
  )
}

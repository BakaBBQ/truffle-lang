name := "tlaf"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
resolvers += Resolver.sonatypeRepo("releases")
resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"

libraryDependencies += "javax.servlet" % "javax.servlet-api" % "3.1.0"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.2-SNAPSHOT" changing()
libraryDependencies += "io.argonaut" %% "argonaut-scalaz" % "6.2-SNAPSHOT" changing()
libraryDependencies += "io.argonaut" %% "argonaut-monocle" % "6.2-SNAPSHOT" changing()

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.3.4"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % "0.2.1",
  "io.circe" %% "circe-generic" % "0.2.1",
  "io.circe" %% "circe-parse" % "0.2.1"
)

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finch-core" % "0.9.2",
  "com.github.finagle" %% "finch-circe" % "0.9.2"
)

libraryDependencies +=
  "com.github.alexarchambault" %% "argonaut-shapeless_6.1" % "1.0.0-M1"

libraryDependencies ++= Seq(
  "eu.timepit" %% "refined"            % "0.3.3",
  "eu.timepit" %% "refined-scalaz"     % "0.3.3",         // optional, JVM only
  "eu.timepit" %% "refined-scodec"     % "0.3.3",         // optional
  "eu.timepit" %% "refined-scalacheck" % "0.3.3" % "test" // optional
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.5.0"

enablePlugins(JavaAppPackaging)
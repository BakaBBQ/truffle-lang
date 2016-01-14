name := "tlaf"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"

libraryDependencies += "javax.servlet" % "javax.servlet-api" % "3.1.0"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.2-SNAPSHOT" changing()
libraryDependencies += "io.argonaut" %% "argonaut-scalaz" % "6.2-SNAPSHOT" changing()
libraryDependencies += "io.argonaut" %% "argonaut-monocle" % "6.2-SNAPSHOT" changing()

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.3.4"

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finch-core" % "0.9.2",
  "com.github.finagle" %% "finch-circe" % "0.9.2",
  "io.circe" %% "circe-generic" % "0.2.1"
)

enablePlugins(JavaAppPackaging)

mainClass := Some("TalfServer")
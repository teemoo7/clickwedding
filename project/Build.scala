import sbt._
import Keys._
import play.Project._
import com.typesafe.sbt.SbtAtmosPlay.atmosPlaySettings

object ApplicationBuild extends Build {

    val appName         = "wedding"
    val appVersion      = "1.0"

    val appDependencies = Seq(
      jdbc,
      anorm,
      "mysql" % "mysql-connector-java" % "5.1.18", // mySQL driver
      "org.owasp" % "antisamy" % "1.4", // antisamy lib
//      "securesocial" %% "securesocial" % "master-SNAPSHOT", // secure social login (google, facebook, twitter, linkedin)
      "securesocial" %% "securesocial" % "2.1.0", // secure social login (google, facebook, twitter, linkedin)
      "com.typesafe" %% "play-plugins-mailer" % "2.1-RC2", // mailer
//      "com.typesafe" %% "play-plugins-mailer" % "2.0.4", // mailer
      "org.apache.poi" % "poi" % "3.8", // Excel Apache POI
      "com.amazonaws" % "aws-java-sdk" % "1.4.1", // Amazon Web Services
      //"pdf" % "pdf_2.10" % "0.5", // PDF files
      ("pdf" %% "pdf" % "0.5").exclude("org.xhtmlrenderer", "core-renderer"),
      "org.xhtmlrenderer" % "flying-saucer-pdf" % "9.0.2",
      "net.glxn" % "qrgen" % "1.3", // QRCode generation,
//      "com.yammer.metrics" % "metrics-core" % "3.0.1", // Metrics performance profiler with VisualVM
//      "com.newrelic.agent.java" % "newrelic-agent" % "3.4.2", // New relic monitoring (works with Heroku as well)
      "com.jolbox" % "bonecp" % "0.8.0.RELEASE", // BoneCP DB connection pool with DBC (timeout bug safe)
      "com.mchange" %%  "c3p0-play"  % "0.1.1" // C3P0-play plugin instead of boneCP
      //"com.typesafe.play" %% "play-slick" % "0.6.0.1" // Slick plugin
    )

    val main = play.Project(appName, appVersion, appDependencies).settings(
      // Typesafe console via SBT plugin Atmos => http://localhost:9900/#play/details
      atmosPlaySettings ++
      Seq(
      // Add your own project settings here
      resolvers += Resolver.url("sbt-plugin-realeases", url("http://repo.scala-sbt.org/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns),
//      resolvers += Resolver.url("sbt-plugin-snapshots", url("http://repo.scala-sbt.org/scalasbt/sbt-plugin-snapshots/"))(Resolver.ivyStylePatterns),
      resolvers += Resolver.url("Violas Play Modules", url("http://www.joergviola.de/releases/"))(Resolver.ivyStylePatterns)
      //resolvers += "sonatype-snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
//      resolvers += Resolver.url("QRGen", url("http://kenglxn.github.com/QRGen/repository"))(Resolver.ivyStylePatterns)
      //resolvers += Resolver.url("QRGen", url("http://kenglxn.github.com/QRGen/repository"))(Resolver.ivyStylePatterns)
      ): _*
    )

}

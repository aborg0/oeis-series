name := "oeis-series"

version := "0.1"

scalaVersion := "2.13.2"
ThisBuild / scalaVersion := "2.13.2"

lazy val root = project.in(file(".")).
  aggregate(oeisSeries.js, oeisSeries.jvm).
  settings(
    publish := {},
    publishLocal := {},
  )

val monocleVersion = "2.0.4"

lazy val oeisSeries = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "oeis-series",
    version := "0.1-SNAPSHOT",
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.3.0",
    libraryDependencies += "com.github.julien-truffaut" %%% "monocle-core" % monocleVersion,
    libraryDependencies += "com.github.julien-truffaut" %%% "monocle-macro" % monocleVersion,
    libraryDependencies += "com.github.julien-truffaut" %%% "monocle-law" % monocleVersion % "test",
    libraryDependencies += "org.scalactic" %%% "scalactic" % "3.1.1",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.1.1" % "test",
    libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-14" % "3.1.1.1" % "test",
  ).
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := true,
  )

lazy val oeisSeriesJS = oeisSeries.js
  .enablePlugins(ScalaJSBundlerPlugin)
  .enablePlugins(ScalablyTypedConverterPlugin)
  .settings(
    useYarn := true,
    // This is a bit of mess. In case you face problems resolving on IDEA import, probably a wrong state was cached.
    // In that case please add a new dependency and reimport
    // Do not forget to run oeisSeriesJS/fastOptJS::webpack too after dependency changes!
    Compile / npmDependencies ++= Seq(
      "chart.js" -> "2.9.3",
      "@types/chart.js" -> "2.9.18",
    ),
    Compile / npmDevDependencies ++= Seq(
    ),
    version in webpack := "4.42.1",
    libraryDependencies += "com.raquo" %%% "laminar" % "0.9.0",
//    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0",
  )
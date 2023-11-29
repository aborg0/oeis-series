name := "oeis-series"

version := "0.1"

scalaVersion := "3.3.1"
ThisBuild / scalaVersion := "3.3.1"

lazy val root = project.in(file(".")).
  aggregate(oeisSeries.js, oeisSeries.jvm).
  settings(
    publish := {},
    publishLocal := {},
  )

val monocleVersion = "3.2.0"

lazy val oeisSeries = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "oeis-series",
    version := "0.1-SNAPSHOT",
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "3.0.2",
    libraryDependencies += "dev.optics" %%% "monocle-core" % monocleVersion,
    libraryDependencies += "dev.optics" %%% "monocle-macro" % monocleVersion,
    libraryDependencies += "dev.optics" %%% "monocle-law" % monocleVersion % "test",
    libraryDependencies += "org.scalactic" %%% "scalactic" % "3.2.17",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.17" % "test",
    libraryDependencies += "org.scalatestplus" %%% "scalacheck-1-17" % "3.2.17.0" % "test",
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
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    // This is a bit of mess. In case you face problems resolving on IDEA import, probably a wrong state was cached.
    // In that case please add a new dependency and reimport
    // Do not forget to run oeisSeriesJS/fastOptJS::webpack too after dependency changes!
    Compile / npmDependencies ++= Seq(
      "chart.js" -> "2.9.4",
      "@types/chart.js" -> "2.9.28",
    ),
    Compile / npmDevDependencies ++= Seq(
    ),
//    webpack / version := "4.42.1",
    libraryDependencies += "com.raquo" %%% "laminar" % "16.0.0",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.8.0",
  )
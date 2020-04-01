name := "oeis-series"

version := "0.1"

scalaVersion := "2.13.1"
ThisBuild / scalaVersion := "2.13.1"

lazy val root = project.in(file(".")).
  aggregate(oeisSeries.js, oeisSeries.jvm).
  settings(
    publish := {},
    publishLocal := {},
  )


lazy val oeisSeries = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "oeis-series",
    version := "0.1-SNAPSHOT",
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "2.2.4",
    libraryDependencies += "com.github.julien-truffaut" %%% "monocle-core" % "2.0.4",
    libraryDependencies += "org.scalactic" %%% "scalactic" % "3.1.1",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.1.1" % "test",
  ).
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := true,
//    libraryDependencies += "org.scalablytyped" %%% "vega-typings" % "0.15.0-f9e28c",
//    libraryDependencies += ScalablyTyped.V.vega,
//    libraryDependencies += ScalablyTyped.D.d3,
  )

lazy val oeisSeriesJS = oeisSeries.js
  .enablePlugins(ScalaJSBundlerPlugin)
  .enablePlugins(ScalablyTypedConverterPlugin)
  .settings(
    //.useYarn := true
    Compile / npmDependencies ++= Seq(
      "vega" -> "5.10.1",
      "vega-typings" -> "0.15.0",
      //  "@types/vega" -> "5.10.1"
    )
  )
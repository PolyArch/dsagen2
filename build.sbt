name := "dsagen2"
version := "2.0.0"
scalaVersion := "2.12.10"
scalacOptions ++= Seq(
      "-Xsource:2.11",
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-language:postfixOps",
      )
libraryDependencies ++= Seq(
        "org.scalatest"     %% "scalatest"         % "3.2.0" % "test",
        "org.scalanlp"      %% "breeze"            % "1.1",
        "com.typesafe.play" %% "play-json"         % "2.9.2",
        "org.reflections"   %  "reflections"       % "0.9.12",
        "org.scala-graph"   %% "graph-core"        % "1.13.1",
        "edu.berkeley.cs"   %% "chisel3"           % "3.4.+",
        "edu.berkeley.cs"   %% "chiseltest"        % "0.3.3" % "test",
        "edu.berkeley.cs"   %% "chisel-iotesters"  % "1.5.+",
        "edu.berkeley.cs"   %% "rocketchip"        % "1.2.+"
        )
resolvers ++= Seq(
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases"),
      Resolver.mavenLocal
      )
dependsOn(xiangshan_hardfloat % "compile->compile")
addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.4.+" cross CrossVersion.full)
addCompilerPlugin("org.scalamacros" % "paradise"       % "2.1.1" cross CrossVersion.full)

lazy val xiangshan_hardfloat = (project in file("hardfloat"))
        .settings(addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.4.+" cross CrossVersion.full))

	

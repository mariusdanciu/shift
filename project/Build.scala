import sbt._
import Keys._
import io._

object ShiftBuild extends Build {

  val distDir = new File("./dist")
  val distSrcDir = new File("./distSrc")
  val libDir = distDir / "lib"
  val buildPropsFile = new File("./build.properties")


  val distShiftCommon = TaskKey[File]("distShiftCommon", "")
  val distShiftEngine = TaskKey[File]("distShiftEngine", "")
  val distShiftTemplate = TaskKey[File]("distShiftTemplate", "")
  val distShiftHtml = TaskKey[File]("distShiftHtml", "")
  val distShiftHttp = TaskKey[File]("distShiftHttp", "")

  val dist = TaskKey[Unit]("dist", "")
  val distSrc = TaskKey[Unit]("distSrc", "")
  val inc = TaskKey[Unit]("inc", "")

  val incSetting = inc := {
    buildProps.setProperty("build", buildProps.getProperty("build").toInt + 1 + "");
    IO.write(buildProps, "", buildPropsFile)
  }

  val distSrcSetting = distSrc <<= (scalaVersion, version) map { (sv, v) =>
    IO.delete(distSrcDir)
    IO.createDirectory(distSrcDir)
    IO.copyDirectory(new File("shift-common") / "src", distSrcDir / "shit-common" / "src")
    IO.copyDirectory(new File("shift-engine") / "src", distSrcDir / "shit-engine" / "src")
    IO.copyDirectory(new File("shift-html") / "src", distSrcDir / "shit-html" / "src")
    IO.copyDirectory(new File("shift-template") / "src", distSrcDir / "shit-template" / "src")
    IO.copyDirectory(new File("shift-http") / "src", distSrcDir / "shit-http" / "src")
 
    IO.copyFile(new File("./LICENSE.txt"), distSrcDir / "LICENSE.txt");

    TarGzBuilder.makeTarGZ("target/shift_src_" + sv + "_" + v + "_.tar.gz", "./distSrc" )
 
  }



  val buildProps = {
    import java.util.Properties
    val prop = new Properties()
    IO.load(prop, buildPropsFile)
    prop
  }

  IO.delete(distDir)
  IO.createDirectory(distDir)
  IO.createDirectory(libDir)


  val distSetting = dist <<= (target, managedClasspath in Runtime, scalaVersion, version, 
                              distShiftCommon in shift_common, 
                              distShiftEngine in shift_engine, 
                              distShiftTemplate in shift_template, 
			      distShiftHtml in shift_html, 
			      distShiftHttp in shift_http, 
			      distSrc) map { (target, cp, sv, v, common, engine, template, html, http, src) => {
      println("dist > shift")

      IO.copyFile(common, libDir / common.name);
      IO.copyFile(engine, libDir / engine.name);
      IO.copyFile(template, libDir / template.name);
      IO.copyFile(html, libDir / html.name);
      IO.copyFile(http, libDir / http.name);
      IO.copyFile(new File("./LICENSE.txt"), distDir / "LICENSE.txt");

      TarGzBuilder.makeTarGZ("target/shift_" + sv + "_" + v + "_.tar.gz", "./dist")
    }
  }

  val distShiftCommonSetting = distShiftCommon <<= (target, managedClasspath in Runtime, publishLocal, packageBin in Compile) map {
    (target, cp, _, pack) => {
        println("dist > shiftcommon")
       
        for {jar <- cp} {
          IO.copyFile(jar.data, libDir / jar.data.name);
        }

	pack
    }
  }

  val distShiftEngineSetting = distShiftEngine <<= (target, managedClasspath in Runtime, publishLocal, packageBin in Compile) map {
    (target, cp, _, pack) => {
        println("dist > shiftengine")
        for {jar <- cp} {
          IO.copyFile(jar.data, libDir / jar.data.name);
        }
	pack
    }
  }

  val distShiftTemplateSetting = distShiftTemplate <<= (target, managedClasspath in Runtime, publishLocal, packageBin in Compile) map {
    (target, cp, _, pack) => {
        println("dist > shifttemplate")
        for {jar <- cp} {
          IO.copyFile(jar.data, libDir / jar.data.name);
        }
	pack
    }
  }

  val distShiftHtmlSetting = distShiftHtml <<= (target, managedClasspath in Runtime, publishLocal, packageBin in Compile) map {
    (target, cp, _, pack) => {
        println("dist > shifthtml")
        for {jar <- cp} {
          IO.copyFile(jar.data, libDir / jar.data.name);
        }
	pack
    }
  }

  val distShiftHttpSetting = distShiftHttp <<= (target, managedClasspath in Runtime, publishLocal, packageBin in Compile) map {
    (target, cp, _, pack) => {
        println("dist > shifthttp")
        for {jar <- cp} {
          IO.copyFile(jar.data, libDir / jar.data.name);
        }
	pack
    }
  }

  lazy val root = Project(id = "shift",
                          base = file("."),
                          settings = Defaults.defaultSettings ++ Seq(incSetting, distSrcSetting, distSetting, distShiftCommonSetting, 
                                     distShiftEngineSetting, distShiftTemplateSetting, 
                                     distShiftHtmlSetting, distShiftHttpSetting)) aggregate(shift_common, shift_engine, 
				     shift_template, shift_html, shift_http)

  lazy val shift_common = Project(id = "shift-common",
				  base = file("shift-common"),
                                  settings = Defaults.defaultSettings ++ Seq(distShiftCommonSetting))

  lazy val shift_engine = Project(id = "shift-engine",
				  base = file("shift-engine"),
                                  settings = Defaults.defaultSettings ++ Seq(distShiftEngineSetting)) dependsOn (shift_common, shift_http, shift_template)

  lazy val shift_template = Project(id = "shift-template",
				    base = file("shift-template"),
                                    settings = Defaults.defaultSettings ++ Seq(distShiftTemplateSetting)) dependsOn (shift_common)

  lazy val shift_html = Project(id = "shift-html",
	                        base = file("shift-html"),
                                settings = Defaults.defaultSettings ++ Seq(distShiftHtmlSetting)) dependsOn (shift_common)

  lazy val shift_http = Project(id = "shift-http",
	                        base = file("shift-http"),
                                settings = Defaults.defaultSettings ++ Seq(distShiftHttpSetting)) dependsOn (shift_common)


}

object TarGzBuilder {
  
  import java.io._
  import org.apache.commons.compress.archivers.tar._
  import org.apache.commons.compress.compressors.gzip._
  
  
  def makeTarGZ(name: String, src: String) {
     val tOut = new TarArchiveOutputStream(new GzipCompressorOutputStream(new BufferedOutputStream(new FileOutputStream(new File(name)))))
     try {
       populateTarGz(tOut, src)
     } finally {
       tOut.close();
     } 
  }
  
  
  def populateTarGz(tOut: TarArchiveOutputStream, path: String, base: String = null) {
    val f = new File(path);
    val entryName = if (base == null) "shift" else (base + f.getName());
    val tarEntry = new TarArchiveEntry(f, entryName);
    tOut.putArchiveEntry(tarEntry);

    if (f.isFile()) {
      IO.transfer(f, tOut);
      tOut.closeArchiveEntry();
    } else {
      tOut.closeArchiveEntry();
      val children = f.listFiles();
      if (children != null){
        for (child <- children) {
          populateTarGz(tOut, child.getAbsolutePath(), entryName + "/");
        }
      }
    }
  }
  
}


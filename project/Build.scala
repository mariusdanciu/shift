import sbt._
import Keys._
import io._

object ShiftNettyBuild extends Build {

  val distDir = new File("./dist")
  val libDir = distDir / "lib"

  val distShiftCommon = TaskKey[File]("distShiftCommon", "")
  val distShiftEngine = TaskKey[File]("distShiftEngine", "")
  val distShiftTemplate = TaskKey[File]("distShiftTemplate", "")
  val distShiftNetty = TaskKey[File]("distShiftNetty", "")
  val distShiftHtml = TaskKey[File]("distShiftHtml", "")
  val distShiftDemo = TaskKey[File]("distShiftDemo", "")
  val dist = TaskKey[Unit]("dist", "")

  val buildProps = {
    import java.util.Properties
 
    val prop = new Properties()
    val file = new File("./build.properties")
    
    IO.load(prop, file)
    prop.setProperty("build", prop.getProperty("build").toInt + 1 + "");
    IO.write(prop, "", file)
    prop
  }

  IO.delete(distDir)
  IO.createDirectory(distDir)
  IO.createDirectory(libDir)


  val distSetting = dist <<= (target, managedClasspath in Runtime, scalaVersion, version, 
                              distShiftCommon in shift_common, distShiftEngine in shift_engine, 
                              distShiftTemplate in shift_template, distShiftNetty in shift_netty, 
                              distShiftHtml in shift_html, distShiftDemo in shift_demo) map {
    (target, cp, sv, v, common, engine, template, netty, html, demo) => {
      println("dist > shift")

      IO.copyFile(common, libDir / common.name);
      IO.copyFile(engine, libDir / engine.name);
      IO.copyFile(template, libDir / template.name);
      IO.copyFile(netty, libDir / netty.name);
      IO.copyFile(html, libDir / html.name);
      IO.copyFile(demo, libDir / demo.name);

      TarGzBuilder.makeTarGZ("target/shift_" + sv + "_" + v + "_.tar.gz")
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

  val distShiftNettySetting = distShiftNetty <<= (target, managedClasspath in Runtime, publishLocal, packageBin in Compile) map {
    (target, cp, _, pack) => {
        println("dist > shiftnetty")

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

  val distShiftDemoSetting = distShiftDemo <<= (target, managedClasspath in Runtime, publishLocal, packageBin in Compile) map {
    (target, cp, _, pack) => {
        println("dist > shiftdemo")
	pack
    }
  }


  lazy val root = Project(id = "shift",
                          base = file("."),
                          settings = Defaults.defaultSettings ++ Seq(distSetting, distShiftCommonSetting, 
                                     distShiftEngineSetting, distShiftTemplateSetting, distShiftNettySetting,
                                     distShiftHtmlSetting, distShiftDemoSetting)) aggregate(shift_common, shift_engine, shift_netty, shift_template, shift_html)

  lazy val shift_common = Project(id = "shift-common",
				  base = file("shift-common"),
                                  settings = Defaults.defaultSettings ++ Seq(distShiftCommonSetting))

  lazy val shift_engine = Project(id = "shift-engine",
				  base = file("shift-engine"),
                                  settings = Defaults.defaultSettings ++ Seq(distShiftEngineSetting)) dependsOn (shift_common, shift_template)

  lazy val shift_template = Project(id = "shift-template",
				    base = file("shift-template"),
                                    settings = Defaults.defaultSettings ++ Seq(distShiftTemplateSetting)) dependsOn (shift_common)

  lazy val shift_netty = Project(id = "shift-netty",
				 base = file("shift-netty"),
                                 settings = Defaults.defaultSettings ++ Seq(distShiftNettySetting)) dependsOn (shift_engine)

  lazy val shift_html = Project(id = "shift-html",
	                        base = file("shift-html"),
                                settings = Defaults.defaultSettings ++ Seq(distShiftHtmlSetting)) dependsOn (shift_common)


  lazy val shift_demo = Project(id = "demo",
				base = file("examples/demo"),
                                settings = Defaults.defaultSettings ++ Seq(distShiftDemoSetting)) dependsOn (shift_netty)


}

object TarGzBuilder {
  
  import java.io._
  import org.apache.commons.compress.archivers.tar._
  import org.apache.commons.compress.compressors.gzip._
  
  
  def makeTarGZ(name: String) {
     val tOut = new TarArchiveOutputStream(new GzipCompressorOutputStream(new BufferedOutputStream(new FileOutputStream(new File(name)))))
     try {
       populateTarGz(tOut, "./dist")
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

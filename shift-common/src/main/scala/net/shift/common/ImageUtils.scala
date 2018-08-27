package net.shift.common

import java.awt.Image
import java.awt.image.BufferedImage
import java.io.{BufferedInputStream, ByteArrayInputStream, File}
import javax.imageio.stream.{FileImageOutputStream, ImageOutputStream}
import javax.imageio.{IIOImage, ImageIO, ImageWriter}

import net.shift.io.LocalFileSystem.mkdir
import net.shift.io.{IO, LocalFileSystem}

import scala.util.Try

object ImageUtils {

  def resizeImage(targetWidth: Int, inputImage: Array[Byte], outputPath: String): Try[String] = Try {
    val img = ImageIO.read(new ByteArrayInputStream(inputImage))

    val h = img.getHeight
    val w = img.getWidth

    val percent = targetWidth * 100 / w

    val nw = targetWidth
    val nh = percent * h / 100

    val srcSized = img.getScaledInstance(nw, nh, Image.SCALE_SMOOTH)

    val dest = new BufferedImage(nw, nh, BufferedImage.TYPE_INT_ARGB)
    val graphics = dest.getGraphics

    graphics.drawImage(srcSized, 0, 0, null)

    val writer: ImageWriter = ImageIO.getImageWritersByMIMEType("image/png").next

    val params = writer.getDefaultWriteParam

    mkdir(Path(outputPath).dropLast)

    val toFs = new FileImageOutputStream(new File(outputPath))

    try {
      writer.setOutput(toFs)
      val image = new IIOImage(dest, null, null)
      writer.write(null, image, params)
      toFs.flush()

      outputPath
    } finally {
      if (toFs != null) toFs.close()
      graphics.dispose()
      writer.dispose()
    }
  }

  def resizeImage(targetWidth: Int, inputPath: String, outputPath: String): Try[String] = {
    for {
      bin <- LocalFileSystem.reader(Path(inputPath))
      array <- IO.producerToArray(bin)
      out <- resizeImage(targetWidth, array, outputPath)
    } yield {
      out
    }
  }

}

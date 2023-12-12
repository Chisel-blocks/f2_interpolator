// See LICENSE_AALTO.txt for license details

package f2_interpolator.config

import net.jcazevedo.moultingyaml._
import net.jcazevedo.moultingyaml.DefaultYamlProtocol._
import scala.math.BigInt
import scala.io.Source
import chisel3._

import hb_interpolator.config
import cic_interpolator.config

case class F2Config(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:         Int,
  gainBits:           Int,
  hb1_config:         HbConfig,
  hb2_config:         HbConfig,
  hb3_config:         HbConfig,
  cic3_config:        CicConfig
)

object F2Config {
  implicit val f2ConfigFormat = yamlFormat3(F2Config.apply)

  // TODO: Update this to always match the major version number of the release
  val syntaxVersion = 2

  /** Exception type for FIR config parsing errors */
  class F2ConfigParseException(msg: String) extends Exception(msg)

  /** Type for representing error return values from a function */
  case class Error(msg: String) {
    /** Throw a parsing exception with a debug message. */
    def except() = { throw new F2ConfigParseException(msg) }

    /** Abort program execution and print out the reason */
    def panic() = {
      System.err.println(msg)
      System.exit(-1)
    }
  }

  /** parse legal syntax version from config yaml AST */
  private[config] def parseSyntaxVersion(yamlAst: YamlValue): Either[BigInt,Error] = {
    // get version number as an integer
    val version: BigInt = yamlAst.asYamlObject.fields.get(YamlString("syntax_version")) match {
      case Some(version) => version match {
        case maybeDecimal: YamlNumber => maybeDecimal.asInstanceOf[YamlNumber].value.toBigIntExact match {
          case Some(integer) => integer
          case None => return Right(Error(s"Top-level key `syntax_version` must have an integer value. $version is not!"))
        }
        case _ => return return Right(Error(s"Top-level key `syntax_version` must have an integer value. $version is not!"))
      }
      case None => return Right(Error("Missing required top-level key: `syntax_version`."))
    }
    if (syntaxVersion != version)
      return Right(Error(s"Unsupported syntax version: $version.\n- Supported versions: $syntaxVersion"))
    Left(version)
  }

  def loadFromFile(filename: String): Either[F2Config, Error] = {
    println(s"\nLoading fir configuration from file: $filename")
    var fileString: String = ""
    try {
      val bufferedSource = Source.fromFile(filename)
      fileString = bufferedSource.getLines.mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }
    
    // print file contents as troubleshooting info
    println("\nYAML configuration file contents:")
    //println(s"```\n$fileString\n```")

    // Determine syntax version
    val yamlAst = fileString.parseYaml
    val syntaxVersion = parseSyntaxVersion(yamlAst)
    syntaxVersion match {
      case Left(value) => ()
      case Right(err) => return Right(err)
    }

    // Parse FirConfig from YAML AST
    val f2_config = yamlAst.convertTo[F2Config]

    val config = new F2Config(f2_config.syntax_version, f2_config.resolution, f2_config.gainBits)

    println("resolution:")
    println(config.resolution)

    println("gainBits:")
    println(config.gainBits)

    Left(config)
  }
}

// See LICENSE_AALTO.txt for license details

package f2_interpolator.config

import net.jcazevedo.moultingyaml._
import net.jcazevedo.moultingyaml.DefaultYamlProtocol._
import scala.math.BigInt
import scala.io.Source
import chisel3._

import hb_interpolator.config.{HbConfig}
import cic_interpolator.config.{CicConfig}

case class F2Generic(
  syntax_version:     Option[Int], // None for scala instantiation
  resolution:         Int,
  gainBits:           Int
)

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
  implicit val f2GenericFormat = yamlFormat3(F2Generic)

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

  def loadFromFile(
    f2_file: String, 
    hb1_file: String, 
    hb2_file: String, 
    hb3_file: String, 
    cic3_file: String
    ): Either[F2Config, Error] = {

    println(s"\nLoading f2 configuration from file: $f2_file")
    var f2_fileString: String = ""
    try {
      val bufferedSource = Source.fromFile(f2_file)
      f2_fileString = bufferedSource.getLines().mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }
    
    // print file contents as troubleshooting info
    println("\nYAML configuration file contents:")
    //println(s"```\n$fileString\n```")

    // Determine syntax version
    val F2yamlAst = f2_fileString.parseYaml

    val syntaxVersion = parseSyntaxVersion(F2yamlAst)
    syntaxVersion match {
      case Left(value) => ()
      case Right(err) => return Right(err)
    }

    var hb1_config: Option[HbConfig] = None
    var hb2_config: Option[HbConfig] = None
    var hb3_config: Option[HbConfig] = None
    var cic3_config: Option[CicConfig] = None

    HbConfig.loadFromFile(hb1_file) match {
        case Left(config) => {
            hb1_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 hb1 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    HbConfig.loadFromFile(hb2_file) match {
        case Left(config) => {
            hb2_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 hb2 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    HbConfig.loadFromFile(hb3_file) match {
        case Left(config) => {
            hb3_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 hb3 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    CicConfig.loadFromFile(cic3_file) match {
        case Left(config) => {
            cic3_config = Some(config)
        }
        case Right(err) => {
            System.err.println(s"\nCould not load F2 cic3 configuration from file:\n${err.msg}")
            System.exit(-1)
        }
    }

    // Parse FirConfig from YAML AST
    val f2_config = F2yamlAst.convertTo[F2Generic]

    val config = new F2Config(
	    f2_config.syntax_version, 
	    f2_config.resolution, 
	    f2_config.gainBits,
        hb1_config.get,
        hb2_config.get,
        hb3_config.get,
        cic3_config.get
    )

    println("resolution:")
    println(config.resolution)

    println("gainBits:")
    println(config.gainBits)

    Left(config)
  }
}

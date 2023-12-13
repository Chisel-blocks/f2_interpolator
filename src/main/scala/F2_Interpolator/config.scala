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
  implicit val HbConfigFormat = yamlFormat4(HbConfig.apply)
  implicit val CicConfigFormat = yamlFormat4(CicConfig.apply)

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

  def loadFromFiles(f2file: String, hb1file: String, hb2file: String, hb3file: String, cic3file: String): Either[F2Config, Error] = {
    println(s"\nLoading f2 configuration from file: $f2file")
    var f2fileString: String = ""
    try {
      val bufferedSource = Source.fromFile(f2file)
      f2fileString = bufferedSource.getLines().mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }

    println(s"\nLoading hb1 configuration from file: $hb1file")
    var hb1fileString: String = ""
    try {
      val bufferedSource = Source.fromFile(hb1file)
      hb1fileString = bufferedSource.getLines().mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }

    println(s"\nLoading hb2 configuration from file: $hb2file")
    var hb2fileString: String = ""
    try {
      val bufferedSource = Source.fromFile(hb2file)
      hb2fileString = bufferedSource.getLines().mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }

    println(s"\nLoading hb3 configuration from file: $hb3file")
    var hb3fileString: String = ""
    try {
      val bufferedSource = Source.fromFile(hb3file)
      hb3fileString = bufferedSource.getLines().mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }

    println(s"\nLoading cic3 configuration from file: $cic3file")
    var cic3fileString: String = ""
    try {
      val bufferedSource = Source.fromFile(cic3file)
      cic3fileString = bufferedSource.getLines().mkString("\n")
      bufferedSource.close
    } catch {
      case e: Exception => return Right(Error(e.getMessage()))
    }
    
    // print file contents as troubleshooting info
    println("\nYAML configuration file contents:")
    //println(s"```\n$fileString\n```")

    // Determine syntax version
    val F2yamlAst = f2fileString.parseYaml
    val HB1yamlAst = hb1fileString.parseYaml
    val HB2yamlAst = hb2fileString.parseYaml
    val HB3yamlAst = hb3fileString.parseYaml
    val CICyamlAst = cic3fileString.parseYaml

    val syntaxVersion = parseSyntaxVersion(F2yamlAst)
    syntaxVersion match {
      case Left(value) => ()
      case Right(err) => return Right(err)
    }

    // Parse FirConfig from YAML AST
    val f2_config = F2yamlAst.convertTo[F2Generic]
    val hb1_config = HB1yamlAst.convertTo[HbConfig]
    val hb2_config = HB2yamlAst.convertTo[HbConfig]
    val hb3_config = HB3yamlAst.convertTo[HbConfig]
    val cic3_config = CICyamlAst.convertTo[CicConfig]

    val config = new F2Config(
	    f2_config.syntax_version, 
	    f2_config.resolution, 
	    f2_config.gainBits,
        hb1_config,
        hb2_config,
        hb3_config,
        cic3_config
    )

    println("resolution:")
    println(config.resolution)

    println("gainBits:")
    println(config.gainBits)

    println("hb1:")
    println(config.hb1_config)

    println("hb2:")
    println(config.hb2_config)

    println("hb3:")
    println(config.hb3_config)

    println("cic:")
    println(config.cic3_config)

    Left(config)
  }
}

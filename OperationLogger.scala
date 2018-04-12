package utils

import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}

object OperationLogger {

  object Mode {
    val KEY = "MODE"
    val MODE_MEM = "MEM"
    val MODE_REDIS = "REDIS"
    var DEFAULT = MODE_MEM
  }

  object Format {
    val KEY = "FORMAT"
    val FORMAT_CSV = "CSV"
    val FORMAT_TSV = "TSV"
    var DEFAULT = FORMAT_CSV
  }

  object Spliter {
    val KEY = "SPLITER"
  }

  object CacheLimitSize {
    val KEY = "MEMORY_LIMIT_SIZE"
    var DEFAULT = 1024 * 1024 * 2
  }

  object CacheLimitTime {
    val KEY = "CACHE_LIMIT_TIME"
    var DEFAULT = 1000*60*15
  }

  object FileLimitSize {
    val KEY = "FILE_LIMIT_SIZE"
    var DEFAULT = 1024 * 1024 * 10
  }

  object FileCheckTime {
    val KEY = "FILE_CHECK_TIME"
    val DEFAULT = 1000*15
  }

  object FileLocation {
    val KEY = "FILE_LOCATION"
    val DEFAULT = "operation_log"
  }

  class OperationLog(val name:String,val fieldsList:Seq[String],conf:Map[String,String]=Map()) {

    val fieldsSet = fieldsList.toSet
    if(fieldsSet.size != fieldsList.size) throw new RuntimeException("field name definition can not repeat")

    private val logLocationDirName = conf.getOrElse(FileLocation.KEY,FileLocation.DEFAULT)
    private var logNumberCode = 0
    private def nextLogFile = logNumberCode += 1
    private def LogFileName = {
      val dir = new File(logLocationDirName)
      if(!dir.exists()) dir.mkdirs()
      s"${logLocationDirName}/${name}_${logNumberCode}.log"
    }

    private val format_csv = Format.FORMAT_CSV
    private val format_tsv = Format.FORMAT_TSV
    private val mode_mem = Mode.MODE_MEM
    private val mode_redis = Mode.MODE_REDIS
    val spliter:String = conf.getOrElse(Spliter.KEY,conf.getOrElse(Format.KEY,Format.DEFAULT) match {
      case `format_csv`=> ","
      case `format_tsv` => "\t"
      case _ => ","
    })
    def format = spliter match {
      case "," => format_csv
      case "\t" => format_tsv
      case _ => spliter
    }
    val mode = conf.getOrElse(Mode.KEY,Mode.DEFAULT)

    private var cache:scala.collection.mutable.Seq[String] = scala.collection.mutable.Seq[String]()

    def write(line:String) = mode match {
      case `mode_mem` => cache = cache :+ line
      case `mode_redis` =>
      case _ =>
    }

    def flush() = mode match {
      case `mode_mem` =>
        val bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(LogFileName,true)))
        cache foreach {line =>
          bw.write(line)
          bw.newLine()
        }
        bw.close()
        cache = scala.collection.mutable.Seq[String]()
        if(new File(LogFileName).length() > FileLimitSize.DEFAULT) nextLogFile
      case `mode_redis` =>
      case _=>
    }

  }

  var CacheSize = 0

  private var fileWriterFlag = false
  private var logsMap:scala.collection.mutable.Map[String,OperationLog] = scala.collection.mutable.Map[String,OperationLog]()

  private val fileWriterThread = new Thread(new Runnable {
    def run(): Unit = {
      var outTime = 0
      while(true)
      {
        if(fileWriterFlag || outTime>CacheLimitTime.DEFAULT){
          logsMap.foreach(_._2.flush())
          CacheSize = 0
          fileWriterFlag = false
          outTime = 0
        }
        Thread.sleep(FileCheckTime.DEFAULT)
        outTime += FileCheckTime.DEFAULT
      }
    }
  })
  fileWriterThread.start()

  def conf(name:String,fields:Seq[String],conf:Map[String,String] = Map()) = logsMap.update(name,new OperationLog(name,fields,conf))

  def log(name:String,values:Seq[String]):Unit = logsMap.get(name) match {
    case Some(logger) =>
      val line = values.mkString(logger.spliter)
      logger.write(line)
      CacheSize += line.getBytes.length
      if(CacheSize > CacheLimitSize.DEFAULT) fileWriterFlag = true
    case None => println("Unknown Logger Name")
  }

  def log(name:String,values:Map[String,String]):Unit = logsMap.get(name) match {
    case Some(logger) => log(name,logger.fieldsList map {field=>values.getOrElse(field,"")})
    case None => println("Unknown Logger Name")
  }

  def flush():Unit = {
    logsMap.foreach(_._2.flush())
    CacheSize = 0
    fileWriterFlag = false
  }

}

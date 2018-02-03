package org.blogoo.scala.ml

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}

import breeze.linalg._
import breeze.numerics._
import breeze.plot._

object Knn {

  def transformArray[T](row:Int,col:Int,arr:Array[T]):Array[T] = {
    if(row*col != arr.length) throw new RuntimeException("row and col can not match array's length")
    val newArr = arr.clone()
    var idx:Int = 0
    for(c <- 0 to col-1){
      for(r <- 0 to row-1){
        newArr.update(idx,arr(c+r*col))
        idx += 1
      }
    }
    newArr
  }

  def classifyKnn(in:DenseVector[Double],dataSet:DenseMatrix[Double],labels:DenseVector[String],k:Long) = {
    val dataSetSize = dataSet.rows
    val diffMat = tile(in.t,1,dataSetSize) - dataSet
    val sqDiffMat = diffMat.map(e => e*e)
    val sqDistances = sum(sqDiffMat,Axis._1)
    val distancesVec = sqrt(sqDistances)
    val sortedIdxes = argsort(distancesVec)
    val countMap = scala.collection.mutable.Map[String,Int]()
    for(i <- 1 to k.toInt) {
      val label = labels(sortedIdxes((i-1).toInt))
      countMap.update(label,countMap.get(label).getOrElse(0) + 1)
    }
    countMap.toList.sortBy(_._2)(Ordering.Int.reverse).head._1
  }

  def readDatingSet(filePath:String) = {
    val br = new BufferedReader(new InputStreamReader(new FileInputStream(filePath)))
    var dataList:scala.collection.mutable.Seq[Double] = scala.collection.mutable.Seq[Double]()
    var labelList:scala.collection.mutable.Seq[String] = scala.collection.mutable.Seq[String]()
    var line:String = br.readLine()
    while(line != null)
    {
      val dataArr = line.split("\t")
      if(dataArr.length == 4){
        for(i <- 0 to 2) dataList = dataList :+ dataArr(i).toDouble
        labelList = labelList :+ dataArr(3)
      }
      line = br.readLine()
    }
    br.close()
    (DenseMatrix.create(dataList.length/3,3,transformArray(dataList.length/3,3,dataList.toArray)),DenseVector(labelList.toArray))
  }

  def autoNorm(dataSet:DenseMatrix[Double]) = {
    val minVals = min(dataSet,Axis._0)
    val maxVals = max(dataSet,Axis._0)
    val ranges = maxVals - minVals
    var normDataSet = (dataSet - tile(minVals,1,dataSet.rows)) /:/ tile(ranges,1,dataSet.rows)
    (normDataSet,ranges,minVals)
  }

  def testDatingKnn() = {
    val (dataSet,labels)  = readDatingSet("assets/knn/datingSet.txt")
    val (normDataSet,ranges,minVals) = autoNorm(dataSet)

    val (testDataSet,testLabels) = readDatingSet("assets/knn/testDatingSet.txt")
    val (testNormDataSet,testRanges,testMinVals) = autoNorm(testDataSet)

    for(k <- 1 to 100){
      var errorCount:Int = 0
      for(r <- 0 to testNormDataSet.rows-1){
        var seq = scala.collection.mutable.Seq[Double]()
        for(c <- 0 to testNormDataSet.cols-1){
          seq = seq :+ testNormDataSet.valueAt(r,c)
        }
        val clsResult = classifyKnn(
          DenseVector(seq.toArray),
          normDataSet,
          labels,
          k
        )
        if(clsResult != testLabels(r)) errorCount += 1
      }
      println(s"K=${k} , error count=${errorCount} , error percent=${errorCount.toDouble/testDataSet.rows.toDouble*100}%")
    }
  }

  def drawFigure2DatingDate = {
    val (dataSet,labels)  = readDatingSet("assets/knn/datingSet.txt")
    var largeDosesXSeq = scala.collection.mutable.Seq[Double]()
    var largeDosesYSeq = scala.collection.mutable.Seq[Double]()
    var dislikeXSeq = scala.collection.mutable.Seq[Double]()
    var dislikeYSeq = scala.collection.mutable.Seq[Double]()
    var smallDosesXSeq = scala.collection.mutable.Seq[Double]()
    var smallDosesYSeq = scala.collection.mutable.Seq[Double]()
    for(r <- 0 to dataSet.rows-1){
      labels.valueAt(r) match {
        case "largeDoses" =>
          largeDosesXSeq = largeDosesXSeq :+ dataSet.valueAt(r,0)
          largeDosesYSeq = largeDosesYSeq :+ dataSet.valueAt(r,1)
        case "didntLike" =>
          dislikeXSeq = dislikeXSeq :+ dataSet.valueAt(r,0)
          dislikeYSeq = dislikeYSeq :+ dataSet.valueAt(r,1)
        case "smallDoses" =>
          smallDosesXSeq = smallDosesXSeq :+ dataSet.valueAt(r,0)
          smallDosesYSeq = smallDosesYSeq :+ dataSet.valueAt(r,1)
        case _=>
      }
    }

    val fig = Figure()
    val p = fig.subplot(0)
    p += scatter(
      DenseVector(largeDosesXSeq.toArray),
      DenseVector(largeDosesYSeq.toArray),
      _=>1000,
      _=>PaintScale.blue)
    p += scatter(
      DenseVector(dislikeXSeq.toArray),
      DenseVector(dislikeYSeq.toArray),
      _=>1000,
      _=>PaintScale.red)
    p += scatter(
      DenseVector(smallDosesXSeq.toArray),
      DenseVector(smallDosesYSeq.toArray),
      _=>1000,
      _=>PaintScale.yellow)
    p.xlabel = "X"
    p.ylabel = "Y"
    fig.saveas("dating.png")
  }

  def readDigitsSet(filename:String) = {
    val trainingDigitsDir = new File(filename)
    val initData = trainingDigitsDir.listFiles() map {f:File =>
      val label = f.getName.split("_")(0)
      val br = new BufferedReader(new InputStreamReader(new FileInputStream(f)))
      val sb = new StringBuilder
      var line = br.readLine()
      while(line != null)
      {
        sb.append(line)
        line = br.readLine()
      }
      br.close()
      var seq = scala.collection.mutable.Seq[Double]()
      sb.toString() foreach {c=>
        seq = seq :+ java.lang.Double.parseDouble(c.toString)
      }
      (seq.toArray,label)
    }
    var dataSeq = scala.collection.mutable.Seq[Array[Double]]()
    var labelSeq = scala.collection.mutable.Seq[String]()
    initData foreach {dl =>
      dataSeq = dataSeq :+ dl._1
      labelSeq = labelSeq :+ dl._2
    }
    val row = dataSeq.length
    val col = dataSeq(0).length
    val dataSet = DenseMatrix.create(row,col,transformArray(row,col,dataSeq.toArray.flatMap(a=>a)))
    val labels = DenseVector(labelSeq.toArray)
    (dataSet,labels)
  }

  def main(args: Array[String]): Unit = {
    val (dataSet,labels) = readDigitsSet("assets/knn/trainingDigits")
    val (testDataSet,testLabels) = readDigitsSet("assets/knn/testDigits")
    var errorCount = 0
    for(r <- 0 to testDataSet.rows-1){
      var inSeq = scala.collection.mutable.Seq[Double]()
      for(c <- 0 to testDataSet.cols-1){
        inSeq = inSeq :+ testDataSet.valueAt(r,c)
      }
      val classifyLabel = classifyKnn(
        DenseVector(inSeq.toArray),
        dataSet,
        labels,
        5
      )
      println(s"classify label is: ${classifyLabel} , real label is ${testLabels.valueAt(r)}")
      if(classifyLabel != testLabels.valueAt(r)) errorCount += 1
    }
    println(s"the error percent is : ${errorCount.toDouble / testDataSet.rows.toDouble * 100}%")
  }
}

package ml

import breeze.linalg.{DenseMatrix, DenseVector}
import play.api.libs.json.{JsObject, JsValue, Json}


object DecisionTree {

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

  def createDemoDataSet = {
    val arr = Array(
      Array("是","是","是鱼"),
      Array("是","是","是鱼"),
      Array("是","否","不是鱼"),
      Array("否","是","不是鱼"),
      Array("否","是","不是鱼")
    )
    val labels = Array("不浮出水面是否可以生存","是否有脚蹼")
    (
      DenseMatrix.create(arr.length,arr(0).length,transformArray(arr.length,arr(0).length,arr.flatMap(a=>a))),
      DenseVector(labels)
    )
  }

  def calcShannonEnt(dataSet:DenseMatrix[String]) = {
    val countMap = scala.collection.mutable.Map[String,Double]()
    for(i <- 0 to dataSet.rows-1) {
      val label:String = dataSet.valueAt(i,dataSet.cols-1)
      countMap.update(label,countMap.getOrElse(label,0.toDouble)+1.toDouble)
    }
    var shannonEnt:Double = 0
    countMap foreach { kv =>
      val prob:Double = kv._2 / dataSet.rows.toDouble
      shannonEnt -= prob * ( Math.log(prob) / Math.log(2) )
    }
    shannonEnt
  }

  def calcGiniImpurity(dataSet:DenseMatrix[String]) = {
    val countMap = scala.collection.mutable.Map[String,Double]()
    for(i <- 0 to dataSet.rows-1){
      val label:String = dataSet.valueAt(i,dataSet.cols-1)
      countMap.update(label,countMap.getOrElse(label,0.toDouble)+1.toDouble)
    }
    var giniImpurity:Double = 0
    countMap foreach {kvA =>
      val probA:Double = kvA._2 / dataSet.rows.toDouble
      countMap foreach {kvB =>
        if(kvA._1 != kvB._1){
          val probB:Double = kvB._2 / dataSet.rows.toDouble
          giniImpurity += probA * probB
        }
      }
    }
    giniImpurity
  }

  def splitDataSet(dataSet:DenseMatrix[String],axis:Int,value:String) = {
    var rowsSeq = scala.collection.mutable.Seq[Array[String]]()
    for(r <- 0 to dataSet.rows-1 ){
      if(dataSet.valueAt(r,axis) == value){
        var colsSeq = scala.collection.mutable.Seq[String]()
        for(c <- 0 to axis-1){
          colsSeq = colsSeq :+ dataSet.valueAt(r,c)
        }
        for(c <- axis+1 to dataSet.cols-1){
          colsSeq = colsSeq :+ dataSet.valueAt(r,c)
        }
        rowsSeq = rowsSeq :+ colsSeq.toArray
      }
    }
    val arr = rowsSeq.toArray
    DenseMatrix.create(arr.length,arr(0).length,transformArray(arr.length,arr(0).length,arr.flatMap(a=>a)))
  }

  def chooseBestFeatureToSplit(dataSet:DenseMatrix[String]) = {
    val numFeatures = dataSet.cols - 1
    val baseEntropy = calcShannonEnt(dataSet)
    var bestInfoGain:Double = 0
    var bestFeature:Int = -1
    for(i <- 0 to numFeatures-1){
      var uniqueValsSet = scala.collection.mutable.Set[String]()
      for(r <- 0 to dataSet.rows-1){
        uniqueValsSet += dataSet.valueAt(r,i)
      }
      var newEntropy:Double = 0
      for(featVal <- uniqueValsSet) {
        val subDataSet = splitDataSet(dataSet,i,featVal)
        val prob:Double = subDataSet.rows.toDouble / dataSet.rows.toDouble
        newEntropy += prob * calcShannonEnt(subDataSet)
      }
      val infoGain = baseEntropy - newEntropy
      if(infoGain > bestInfoGain){
        bestInfoGain = infoGain
        bestFeature = i
      }
    }
    bestFeature
  }

  def majorityCnt(classList:DenseVector[String]) = {
    val countMap:scala.collection.mutable.Map[String,Int] = scala.collection.mutable.Map[String,Int]()
    for(i <- 0 to classList.length-1){
      countMap.update(classList.valueAt(i),countMap.getOrElse(classList.valueAt(i),0)+1)
    }
    var result = ("",0)
    countMap foreach {kv =>
      if(kv._2 > result._2) result = kv
    }
    result._1
  }

  def createTree(dataSet:DenseMatrix[String],labels:DenseVector[String]):JsValue = {
    val classList = DenseVector((for(r <- 0 to dataSet.rows-1) yield {
      dataSet.valueAt(r,dataSet.cols-1)
    }).toArray)
    if(classList.data.toSeq.toSet.size == 1) Json.toJson(classList.valueAt(0))
    else if(dataSet.cols == 1) Json.toJson(majorityCnt(classList))
    else{
      val bestFeat = chooseBestFeatureToSplit(dataSet)
      val bestFeatLabel = labels.valueAt(bestFeat)
      var treeBranches:JsObject = Json.obj()
      val tmpBuffer = labels.toArray.toBuffer
      tmpBuffer.remove(bestFeat)
      val newLabels = DenseVector(tmpBuffer.toArray)
      val uniqueFeatureValues = (for(r <- 0 to dataSet.rows-1) yield {
        dataSet.valueAt(r,bestFeat)
      }).toSet
      for(value <- uniqueFeatureValues){
        val subLabels = newLabels.copy
        treeBranches = treeBranches + (value -> createTree(splitDataSet(dataSet,bestFeat,value),subLabels))
      }
      val tree = Json.obj(bestFeatLabel -> treeBranches)
      tree
    }
  }

  def classify(tree:JsObject,labels:DenseVector[String],testData:DenseVector[String]):String = {
    val feature = tree.fields(0)._1
    val treeBranches = tree(feature).asInstanceOf[JsObject]
    val featureIndex = labels.toArray.indexOf(feature)
    var classLabel = ""
    for(kv <- treeBranches.fields){
      if(kv._1 == testData.valueAt(featureIndex)){
        if(kv._2.isInstanceOf[JsObject]){
          classLabel = classify(kv._2.asInstanceOf[JsObject],labels,testData)
        }else{
          classLabel = kv._2.as[String]
        }
      }
    }
    classLabel
  }

  def main(args: Array[String]): Unit = {
    val tree = createTree(createDemoDataSet._1,createDemoDataSet._2)
    println(classify(tree.asInstanceOf[JsObject],createDemoDataSet._2,DenseVector("否","否")))
  }
}

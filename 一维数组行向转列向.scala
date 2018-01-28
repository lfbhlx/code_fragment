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

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL         # 新しい行列が入ったら逆行列キャッシュは無効化
  }
  
  get <- function() x
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  
  inv <- solve(mat, ...)   # 逆行列を計算
  x$setinverse(inv)        # キャッシュ保存
  inv
}




# Cache the inverse of a matrix
# This function creates a special "matrix" object that can cache its inverse.
# @param x: matrix
# @return list (with getter and setter functions: set(), get(), setInverse(), getInverse())
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL # variable to hold cached inverse
  
  # return list object with getters and setters
  list(
    set = function(y) { x <<- y; inv <<- NULL }, 
    get = function() { x }, 
    setInverse = function(solve){ inv <<- solve }, 
    getInverse = function() { inv }
  )
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# @param o: list (with getter and setter functions: set(), get(), setInverse(), getInverse())
# @return matrix
cacheSolve <- function(o, ...){
  inv <- o$getInverse() # first check if inverse is cached
  
  # return the cached data if not empty
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # else: cache the inverse of the matrix
  mtrx <- o$get() # get the matrix we passed to makeCacheMatrix()
  inv <- solve(mtrx, ...) # invert the matrix
  o$setInverse(inv) # cache the matrix
  inv # return the inverse of the matrix
}

# Unit test for `cacheSolve()`
assert <- function(){
  # make a test matrix 8x8
  x <- (function(n) { 
    i <- 1:n
    1 / outer(i - 1, i, "+") 
  })(8)
  
  fns <- makeCacheMatrix(x) # store reference to our getters and setters
  
  cache <- cacheSolve(fns) #set the cache on 1st call
  cached <- cacheSolve(fns) # get cached inverse on 2nd call
  
  # Test that the 1st and last nodes of the inverted matrix are equal to the 
  # inverted values of the nodes in the original matrix we expect.
  if(isTRUE(all.equal(cached[1,1], 64) && all.equal(cached[8,8], 176679358))){
    message("passed")
  } else{
    message("failed")
  }
  
  cached
}

assert() # run the test


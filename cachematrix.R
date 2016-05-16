## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function create special matrix. 
#It means the matrix not only has value but also other datas. 
#This method has matrix value, matrix inverse value and other functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
#This function create inverse on matrix from makeCacheMatrix method. 
#It's created in 'inv <- solve(mat, ...)' then the inverse attached in 'x$setInverse(inv)'. 
#And also this function check is matrix has inverse ? The function checks by 'if (!is.null(inv))'
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

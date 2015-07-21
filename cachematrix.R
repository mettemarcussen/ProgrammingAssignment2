## Put comments here that give an overall description of what your
## functions do




## An example of an invertible matric, with which I can test if the function works 
##(Source: https://www.mathsisfun.com/algebra/matrix-inverse.html)
x<- matrix(c(4,2,7,6),2,2) #matrix
x_inverse<-matrix (c(0.6,-0.2,-0.7,0.4),2,2)

identity_2x2<-matrix(c(1,0,0,1),2,2)
x%*%x_inverse #True multiplication
solve(x,identity_2x2)

## Write a short comment describing this function.
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { #the function need a matrix to the 
  m <- NULL
  set <- function(y) {
    x <<- y #"the "<<-" operator can be used to assign a value to an object in an environment that is different from the current environment"
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  }

cacheSolve(makeCacheMatrix(x))

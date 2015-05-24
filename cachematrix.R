## caching the inverse of a matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {  
	m<-NULL
  	set<-function(y){ #1.  set the value of the matrix
  	x<<-y
  	m<<-NULL
}
get<-function() x  #2. get the value of the matrix
setmatrix<-function(solve) m<<- solve # 3. get the value of the inverse 	using solve function
getmatrix<-function() m #4.  get the value of the inverse
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix) #returns a list containing functions to cache inverse of a matrix
}

## the cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix, unless the inverse has already been calculated; in this case cacheSolve retrieves the inverted matrix from the cache
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)  # if the inverse has already been calculated, return the cached value
    }
    matrix<-x$get()
    m<-solve(matrix, ...) # if not, calculate the inverse of the matrix
	x$setmatrix(m)
    m # print the calculated inverse
}



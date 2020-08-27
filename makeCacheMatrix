## Put comments here that give an overall description of what your functions do
## makeCacheMatrix is a function that returns a list of functions:
##set : assigns a matrix
##get: retrieves a matrix
##setinverse: assigns and caches an inverse to a matrix
##getinverse: retrieves the inverse of a matrix



makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y = matrix()){
    x <<- y
    v <<- NULL
  }
  get <- function() x 
  setinverse <- function(z=matrix()) v<<- solve(z)
  getinverse <- function() v
  list( set=set,get=get,
        setinverse=setinverse,getinverse=getinverse)
}

##CacheSolve applies the functions listed in makeCacheMatrix
## if  the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
## if the matric has not been solved, its inverse is calculated and cached.

cacheSolve <- function(x,...){
  
  v<-x$getinverse()
  if(!is.null(v)){
    message("getting cached data")
    v
  }
  mdata<-x$get()
  v<-solve(mdata,...)
  x$setinverse()
  
  v
}

##tests

pr<-matrix(c(1,0,0,0,1,0,0,0,2),ncol=3,nrow=3)
pr
solve(pr)
newpr<-makeCacheMatrix(pr)
cacheSolve(newpr)
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
newmat<-matrix(c(4,2,1,4,1,4,2,4,4),3)
newmat
solve(newmat)
system.time(solve(newmat))
mkmat<-makeCacheMatrix(newmat)
system.time(cacheSolve(mkmat))
cacheSolve(mkmat)


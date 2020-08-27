## Put comments here that give an overall description of what your
## functions do

#the first function will create a of a matrix and its inverse. First empty matrix.
#set function. This sets the new inverse. 
#when you call makeCachematrix, there are two possible outcomes: the inverse is cached, or it is not cached, 
#assign values for x(matrix) and its inverse(as an empty matrix # to return null when there is no cached data.)
#"get" as an empty function that takes whatever function is supplied to it and applies it to x.
#the get function should collect its input "x" from the matrix supplied to MCM.
#setinverse solves for the inverse of the matrix through the function solve. it assigns its output to the empty matrix provided, setting its inverse.
#getinverse should be an empty function that takes whatver function is supplied to it and applies it the new value of the inverse above
#collect the items in a list, naming each item to allow for $ indexing

## Write a short comment describing this function

#makecachematrix will take a matrix and check if it has an invese in cache.
#if the inverse is in cache,it is returned. If it is not, it is calculated, and cached.
#the first call is to the inverse cache to chech whether the inverse exists
#i'll use the $index with open brackets so that the function adopts whichever matrix has been supplied and checks for its inverse
#next ill write an if statement to the effect that if an inverse exists, it returns it
#if an inverse doesne exist
#a new object is created and calls get on the inverse supplied
#the empty matrix is assigned the inveser of the new oject
#the inverse is set using setinverse
#the inverse is returned



makeCacheMatrix <- function(x = matrix()) {
 v <- NULL
set <- function(y = matrix()){
  x <<- y
  v <<- NULL
}
get <- function()x 
setinverse <- function(solve(x)) v<<-solve
getinverse <- function()v
list( set=set,get=get,
     setinverse=setinverse,getinverse=getinverse)
}
cacheSolve <- function(x, ...){
  
  v<-x$getinverse()
  if(!is.null(v)){
    v
  }
    mdata<-x$get()
    v<-solve(mdata)
    x$setinverse()
   
    v
}

## Write a short comment describing this function



pr<-matrix(c(1,0,0,0,1,0,0,0,2),ncol=3,nrow=3)

pr
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
makeCacheMatrix(my_matrix)

my_matrix$getInverse()

cacheSolve(my_matrix)

cacheSolve(my_matrix)


my_matrix$getInverse()

my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()

my_matrix$getInverse()

cacheSolve(my_matrix)

cacheSolve(my_matrix)


my_matrix$getInverse()

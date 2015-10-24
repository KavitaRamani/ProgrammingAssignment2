#Matrix inversion is usually a costly computation and there may be 
#some benefit to caching the inverse of a matrix rather than computing 
#it repeatedly 

makeCacheMatrix<-function(x=matrix()){
  
#The first function, makeVector creates a special "vector", which is really 
#a list containing a function to
  
#1.set the matrix
#2.get the matrix
#3.set the inverse of the matrix
#4.get the inverse of the matrix
  
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
    
#This function creates a special "matrix" object that can cache its inverse.
#We assume that the matrix input can be inversed
    
  }
  get<-function()x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

cacheSolve<-function(x,...){
  inv<-x$getinv()
  if(!is.null(inv)) {
    print ("Getting cached data")
    return (inv)
  }
  
#This function computes the inverse of the matrix returned by makeCacheMatrix
#above. If the inverse has already been calculated(and the matrix has not changed)
#the cacheSolve would retrieve the inverse from cache.Else, if the matrix has
#changed, then the new inverse would be computed and overwrites the old cache.
  
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  x
} 


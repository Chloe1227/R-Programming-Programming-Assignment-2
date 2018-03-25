# R-Programming-Programming-Assignment-2
##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean
makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) inv<<-inverse
  getInverse<-function() inv
  list(set=set,get=get,setIverse=setInverse,getInverse=getInverse)
}


##The following function calculates the inverse of the special "matrix" created with the above function. 
cacheSolve<-function(x,...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached matrix")
    return(inv)
  }
  mat<-x$get()
  inv<-inverse(mat,...)
  x$setInverse(inv)
  inv
}
  

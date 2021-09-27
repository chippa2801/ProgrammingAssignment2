##The first function, makeCacheMatrix creates a special Matrix object, that can cache it's inverse.
# it contains
#set,get,getinv,setinv
#library(mass) it allows us to calculate inverse both squared and non-squared matrix.
install.packages("MASS")
library(MASS)
makeCacheMatrix <- function(x = matrix()){
  inv = NULL
  set = function(y){
                    x<<-y
                    inv<<-NULL
                   }
  get    <- function()x
  setinv <- function(inverse)inv<<-inverse
  getinv <- function(){
                      inver <- ginv(x)
                      inver%*%x
                      }
  list(set = set,get = get,
       setinv = setinv,
       getinv = getinv)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSplve <- function(x, ...){
  inv <- x$getinv()
  if(!is.null(inv)){
                    print("getting the cache data!")
                    return(inv)
                    }
 data <- x$get()
 inv <- solve(data,...)
 x$setinv(inv)
 inv
}

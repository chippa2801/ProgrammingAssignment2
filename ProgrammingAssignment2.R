##The first function, makeCacheMatrix creates a special "Matrix" object, that can cache it's inverse.
# it contains
#set,get,getinve,setinve
#library(mass) it allows us to calculate inverse both squared and non-squared matrix.
install.packages("MASS")
library(MASS)
makeCacheMatrix <- function(p = matrix()){
  inve = NULL     # keeping inverse as null at first.
  set = function(q){
                    p<<-q
                    inve<<-NULL
                   }
  get    <- function()p      # this line is to get the matrix.
  setinve <- function(inverse)inve<<-inverse
  getinve <- function(){
                      inver <- ginve(p)
                      inver%*%p    # this is to get the matrix inverse
                      }
  list(set = set,get = get,
       setinve = setinve,
       getinve = getinve)
}
# write some comments describing the function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#cachesolve returns the  cache data.
cacheSolve <- function(p, ...){   ## gets the cache data
  inve <- p$getinve()
  if(!is.null(inve)){
                    print("getting the cache data!")
                    return(inve)    ## it returns the values i.e inverse
                    }
 data <- p$get()
 inve <- solve(data,...)      ## it calculates the inverse values.
 p$setinve(inve)
 inve     # it returns what we are looking for that is inverse of the matrix(p)
}

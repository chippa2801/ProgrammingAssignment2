
#function:

#what is inverse of a matrix ?
#a matrix is invertible if and only a square matrix of order nxn if there exist an another matrix b
#we can find the inverse of matrix not only for square matrix but also for other matrices.the inverse
#of matrix is denoted by A-1.so,we will look into that and moreover in creation of function we try to 
#store the cache in some other environment and we bring it back on.so we can get that.its too hard to 
#compute or takes more time if we have loops in it.rather than computing it again we'll look in the cache.
#so,when we need it again we take it from the cache and its very efficient.

#CACHING THE INVERSE OF A MATRIX:

#productCacheMatrix:

#in this example we are using a package that is MASS which allows us to calcualte inverse for every matrix.either it may be squared or
#non-squared.
#the productCacheMatrix function is used to discover a new "MATRIX" object  which is stored in chippa variable and
#which will perform the most costly 
#computational work that can calculate the cache of a matrix.in this function we go in sidhu's way of producing a
#function.in this we start the matrix as null and we generate a function to obtain the matri$ and next we generate a 
#object that yields the inverse of a matrix.
#so,we use reverse as inverse and set,get to set and return the cache.setreverse and getreverse for us to get the cached inverse of the matrix.
#in this instance we are utilizing a new mechanics or operator  "<<-"  it can be worked when we want to assign a value to an object in a new environment
#which is not our current working environment.
#we generated two new objects that yeilds and sets the inverse of a matrix.


#function # 


library(MASS)
productCacheMatrix <- function(chippa = matrix()) {
  
  # storing a matrix.
  #we are storing a new matrix and initilizing it as NULL.
 reverse <- NULL
 set <- function(sidhu){
                        chippa<<-sidhu
                        reverse<<-NULL
                       }
get <- function()chippa
setreverse <- function(inverse)revere<-inverse
getreverse <- function(){
  
  #yeilds the inverse matrix. 
                         rever<-ginv(chippa)
                         rever%*%chippa
} 
list(set = set,get = get,
     setreverse = setreverse,
     getreverse = getreverse)
}


## Write a breif notes outlining this function.

##CACHEANSWER: ####

#in this we try to calcualte the matrix inversion from that which came from the function productCacheMatrix.
#this has many hurdles because it is very complicated yet it can be done.
#we create a functiopn that obtains the cache data.we use if condition to check whether the inverse is null and 
#to print the inverse value there.atlast we try to get values in the matrix form because we gave it in the matrix
#so,we get that and in the process we calculate the inverse.in this function we use the same variables which we used in the productcachematrix.

cacheanswer <- function(chippa, ...) {
  
  ##yields the cached data.
  
  reverse <- chippa$getreverse()
  if(!is.null(reverse)){
                        print("retriving the cached data:")
                        return(reverse)
  }
  data <- chippa$get()
  reverse <- solve(data,...)
  
  #computes inverse value.
  
  chippa$setreverse(reverse)
  reverse ## yields a matrix that is the inverse of 'chippa'
}

#Thisis the testing of that function(productcachematrix).
x = productCacheMatrix(matrix(1:36,6,6))
x$getreverse()
x$get()
cacheanswer(x)


#####the course is very promising,the d,peng is excellent.the one thing I liked in this course is the statistics with interactive learning which is very unique and we can make progress
#it is very appropriate and i appreciate their work so its so unique idea.
##the way he took the classess are so interesting.
#I learned many things from the course .the rules for creating a function and profiling.
#They have started from basic so, i liked it.#it is the most basic part of the program 
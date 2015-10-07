#This is function "makeCacheMatrix" is making a vector, or a list of of functions

#Functions made in "makeCacheMatrix"
#1. set the value of the matrix
#2. get the value of the matrix
#3. setting the inverse of the matrix
#4. getting the inverse of the matrix

#The result of 'makeCacheMatrix' will return a list of functions with the environments
#It is essentially storing the inputted matrix and inverse matrix


makeCacheMatrix <- function(x = matrix()) { 
  s <- NULL                                   
  set.mat <- function(y){                   
    x <<- y                                 
    s <<- NULL                              
  }                                         
  get.mat <- function(){
    x
  }                            
  set.inv <- function(solve){ 
    s <<- solve 
  }   
  get.inv <- function() {
    s
  }                 
  list(set.mat = set.mat, 
       get.mat = get.mat, 
       set.inv = set.inv, 
       get.inv = get.inv)
}

#The following function is returning or solving for the inverse of a matrix from the above function
#first it is identifying if there is a solved inverse matrix from the above function
#if there is a stored inverse matrix in the cache then it will return it w/o doing any computation
#if there is not an inverse matrix solved for the given data then this function will solve for it 
#and return the inverse matrix and then also cache or 'set' the solved inverse matrix. 


cacheinverse <- function(x, ...){    
  s <- x$get.inv()                  
  if(!is.null(s)) {                  #here we are asking if 's' doesn't equal NULL then print the message and 
    message ("getting cached data")  #return the cached inverse matrix without computing
    return(s)
  }                                  # if the 'if' function finds that 's' is NULL (there is no cached inverse matrix) then this function will do the following:
  data <- x$get.mat()                # assigning the the result of the get.mat function of x from the Cached Matrix function to 'data'  
  s <- solve(data,...)               # solving the inverse matrix of 'data' and assiging it to 's'
  x$set.inv(s)                       # setting 's' as the newly inversed matrix to the 'makeCachedMatrix' function
  s                                  # returning 's', the inversed matrix
}
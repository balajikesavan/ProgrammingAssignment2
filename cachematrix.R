## This function inverses a matrix and then caches the value for future use

#creating a function called makeCacheMatrix and setting the default value of its input as an empty matrix. This is output a list containing various functions 
makeCacheMatrix <- function(x = matrix()){

#creating a variable called temp and setting it NULL. This variable will store the value the cached matrix
  temp <- NULL

#creating a function called set, which will be re-assign the variable temp to NULL if a different vector is passed in the original function
  set <- function(y){
    x <<- y
    temp <<- NULL
  }

#creating a function called get, which will return the matrix originally entered, this is only if the matric entered is different from the matrix whose inverse is cached
  get <- function()x
  
#creating a function called SetInverse, which will Set the value of the temp to the cached inverse
  SetInverse <- function(inverse)temp <<- inverse
  
#creating a function called GetInverse, that will return the value of temp  
  GetInverse <- function()temp
  
#Creating a special vector, containing the 4 functions we just created  
  list(set=set, get=get, SetInverse=SetInverse, GetInverse=GetInverse)
}


#creating a function called Cache Solve that will input, the output of the MakeCacheMatrix
CacheSolve <- function(x, ...){
  
#extracting the value of temp using the GetInverse function
  temp <- x$GetInverse()
  
#if the value of temp is available then the cached value and a relevant message is displayed and the function stops running
  if(!is.null(temp)) {
    message("this is cached info")
    return(temp)
  }

#if the value of temp is not available then the Inverse is calculated using the Solve function
else{
  data <- x$get()
  temp <- solve(data)
  x$SetInverse(temp)
  temp
  }
}

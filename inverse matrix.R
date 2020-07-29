## Function to create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
      
      ## at the beginning there will be no inverse matrix 
      in_mat <- NULL
      
      ##set the value of the matrix
      set<- function(y){
            x <<- y
            in_mat <<-NULL
      }
      
      ## get the value of the matrix
      get <- function()x
      
      ## set the inverse of the matrix 
      set_Inverse<- function(inverse){
            in_mat <<- inverse
      }
      
      ## set the value of the matrix
      get_Inverse <- function()solve(x)
      list(set=set,
           get=get, 
           set_Inverse=set_Inverse,
           get_Inverse=get_Inverse)
      
}


## Function to retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
      
      ##function to return the inverse matrix.
      in_mat <- x$get_Inverse()
      ##checking whether inverse is NULL. 
      if(!is.null(in_mat)){
            message("getting cached data")
            return(in_mat)
      }
      data <- x$get()
      in_mat <- solve(data, ...)
      x$set_Inverse(in_mat)
      
}
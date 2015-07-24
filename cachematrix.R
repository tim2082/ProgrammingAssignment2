
# This function creates a special "matrix" object that can cache its inverse.

  makeCacheMatrix <- function(x = matrix()) {
        
      # set the veriable inv to NULL
      inv <- NULL		
      
      # change the matrix and reset the inverse
      set <- function(y) {	
          x <<- y
          inv <<- NULL
      }
      
      # returns the stored matrix
      get <- function() x	
  
      # change the matrix invers
      setinv <- function(solve) inv <<- solve	
      
      # return stored inverse
      getinv <- function() inv		
      
      # returns a list containing all the get/set functions
      list(set = set, get = get, setinv = setinv, getinv = getinv)
    
  }

    
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache.
  
  cacheSolve <- function(x, ...) {
      
      # cached inverse assignment
      inv <- x$getinv() 
      
      # if it exists, the result comes from the cache
      if(!is.null(inv)) {        
        message("inversed cached data")            
        return(inv)                               
      }
      else {
      
          # it it doesn't exist -> get the matrix
          data <- x$get()                     
          
          # inverse the data
          inv <- solve(data)   
          
          # cache the inverse
          x$setinv(inv)                                     
      }    
          
      inv         
    
  }

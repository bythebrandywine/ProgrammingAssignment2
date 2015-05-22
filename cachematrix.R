# makeCacheMatrix is a function that creates a matrix that can cache its inverse
#
# Arguments:
#   x: A square matrix whose inverse will later be calculated
#   c_inv: A matrix where the inverse of x will be cached
#   y: A square matrix (local variable) that will reset the value of x
#   inverse: A square matrix that is the inverse of some x
#
# makeCacheMatrix stores four functions:
#   1. set: sets a new value for the matrix x stored in the parent function
#   2. get: returns the matrix x stored in the parent function
#   3. setinverse: stores the inverse value of matrix x in the parent function
#   4. getinverse: returns the inverse value of matrix x
#
# Returns:
#   A list of functions that can be used to get and store a matrix and its inverse
#
# Errors:
#   x and y must be square matrices (nrow = ncol) or else an error will be returned
#
makeCacheMatrix <- function(x = matrix()) {   # Function that takes square matrix x as arg
     if(nrow(x) != ncol(x)){                  # If x is not square matrix, function stops
          stop("Please input a square matrix.\n")   # & returns explanatory error
     }
     c_inv <- NULL                            # Creates an empty matrix
     set <- function(y = matrix()) {          # Function that takes square matrix y as arg
          if(nrow(y) != ncol(y)){             # If y is not square matrix, then error
               stop("Please input a square matrix.\n")
          }
          x <<- y                             # Changes value of x in parent environment to y
          c_inv <<- NULL                      # Sets value of cached inverse matrix in parent
     }                                        #    environment to NULL
     get <- function() x                      # Function that returns value of matrix x
     setinverse <- function(inverse) {        # Function that takes matrix inverse as arg
          c_inv <<- inverse                   # Stores inverse value in cached inverse matrix
     }                                        #    in parent environment
     getinverse <- function() {               # Function that returns value of matrix c_inv
          c_inv
     }
     list(                                    # Stores four functions (set, get, setinverse,
          set = set,                          #    getinverse) in a list
          get = get,                          
          setinverse = setinverse,
          getinverse = getinverse
     )
}
#
# cacheSolve is a function that returns the inverse of matrix x
#
# Arguments:
#   x: A square matrix whose inverse will be calculated
#   x_inv: A matrix where the inverse of x will be stored
#   data: Local variable that will take the value of matrix x
#
# Returns:
#   The inverse of matrix x
#
# Errors: 
#   x must be a square matrix (nrow = ncol) or else an error will be returned
#
cacheSolve <- function(x, ...) {          # Function that takes square matrix x as arg
     x_inv <- x$getinverse()              # x_inv takes value getinverse() (c_inv, from above)
     if(!is.null(x_inv)) {                # If x_inv is not NULL (contains value)...
          message("getting cached data")  #    Then print message, and...
          return(x_inv)                   #    Return cached inverse, and exit function
     }                                    # Else... (implicit)
     data <- x$get()                      # data variable takes the value of matrix x
     x_inv <- solve(data, ...)            # x_inv takes value of the inverse of matrix data
     x$setinverse(x_inv)                  # setinverse takes the arg x_inv (sets c_inv above)
     x_inv                                # Return matrix x_inv
}
# This function creates a special “matrix” object that can cache its inverse
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y){
                x <<- y
                m_inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) m_inv <<- inverse
        getinverse <- function() m_inv
        
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)

}

# Return a matrix that is the inverse to makeCacheMatrix(). 
# 1. It first checks if the inverse has already been computed. 
# 2. If so, it gets the result and skips the computation. 
# 3. If not, it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
       m_inv <- x$getinverse()
        
        # if the inverse has already been computed
        if(!is.null(m_inv)){
                message ("getting cached data")
                return(m_inv)
        }
       
       # otherwise, computes the inverse 
        data <- x$get()
        m_inv <- solve(data, ...)
        x$setinverse(m_inv)
        m_inv
}

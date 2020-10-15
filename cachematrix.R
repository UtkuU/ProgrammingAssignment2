## makeCachMatrix creates a special "matrix" object that can cache its inverse: 
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of the solve (inverse) 
## 4. get the value of the solve (inverse) we are assuming that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix())
{
    m_inv <- NULL
    
    set <- function(y)
    {
        x <<- y
        m_inv <<- NULL
    }
    
    get <- function() x
    setm_inv <- function(solve) m_inv <<- solve
    
    getm_inv <- function() m_inv
    
    list(set = set, get = get,
         setm_inv = setm_inv,
         getm_inv = getm_inv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    m_inv <- x$getm_inv()
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data, ...)
    x$setm_inv(m_inv)
    m_inv
}

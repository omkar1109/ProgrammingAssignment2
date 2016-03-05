makeCacheMatrix <- function(x = matrix()) { ## creating a matrix function with object x
        m <- NULL ## defining cache m
        set <- function(y) {
                x <<- y ## assign matrix y as input to variable x in parent environment
                m <<- NULL  ## re-initialize value of m to null in parent environment to null
        }
        get <- function() x  ## return matrix x
        setinverse <- function(inverse) m <<- inverse ## set the value in cache m equal to inverse of matrix x
        getinverse <- function() m ## return inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

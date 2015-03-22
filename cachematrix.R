## Program to compute the inverse of a matrix vector and cache it

## mackeCacheMatrix caches the invese of the matrix vector

makeCacheMatrix <- function(matrix_vec = matrix()) {

        cache<- NULL
        set <- function(x) {
                matrix_vec <<- x;
                cache <<- NULL;
        }
        get <- function() return(matrix_vec);
        setinverse <- function(inverse_vec) cache <<- inverse_vec;
        getinverse <- function() return(cache);
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function cacehSolve() returns the inverse of the matrix
## matrix_vec returned from makeCacheMatrix()

cacheSolve <- function(matrix_vec, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- matrix_vec$getinverse()
        #if cache is available return it
        if(!is.null(inverse)) {
                message("getting cached data...")
                return(inverse)
        }
        data <- matrix_vec$get()
        matrix <- solve(data, ...)
        matrix_vec$setinverse(matrix)
        inverse
}

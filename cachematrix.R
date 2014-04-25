## Invert a matrix. If inverted matrix exists in cahce, return cached value 
## without re-computing

## Create list of functions that can cache inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        ## initialize "m"
        set <- function(y) {
                x <<- y  
                ## Super assign "y" to "x"
                m <<- NULL 
                ## reset "m" since "x" is changed now
        }
        get <- function() x 
        ## Read in "x"
        setInverse <- function(Inverse) m <<- Inverse 
        ## Set the value of "m" with something NOT "NULL"
        getInverse <- function() m 
        ## Read in "m"
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        ## Create the special function list
}


## Inverse of the matrix returned by makeCacheMatrix above. If it already
## exists, return the cached value instead of recalculating

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Read in "m"
        if(!is.null(m)) {
                ## If is "m" is not "NULL", it means "x" has not changed from
                ## last computing, so it will read inverted "x" from cache ("m")
                message("getting cached data")
                return(m)
                ## Return the cached inverted "x" value ("m")
        }
        mtx <- x$get()
        ## If "m" is "NULL", it means "x" has changed, ready to re-compute
        m <- solve(mtx, ...) ## Invert the matrix
        x$setInverse(m) ## Assign the newly computed inverted matrix to "m"
        m
}

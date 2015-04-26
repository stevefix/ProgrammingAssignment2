## Together these functions can be used to return cached inverse of a matrix

## makeCacheMatrix contains functions caching the inverse of a matrix x: 
## $set resets matrix's value and clears the cached inverse inv_mtrx; $get returns it
## $setinv_mtrx computes inverse of matrix and stores it variable inv_mtrx
## $getinv_mtrx returns cached inverse of matrix from variable inv_mtrx



makeCacheMatrix <- function(x = matrix()) {
        inv_mtrx <- NULL
        set <- function(y) {
                x <<- y
                inv_mtrx <<- NULL
        }
        get <- function() x
        setinv_mtrx <- function(solve) inv_mtrx <<- solve
        getinv_mtrx <- function() inv_mtrx
        list(set = set, get = get,
             setinv_mtrx = setinv_mtrx,
             getinv_mtrx = getinv_mtrx)
}


## cacheSolve inverts a matrix by attempting to retrieve its cached inverse using functions in
## makeCacheMatrix.  If cached version exists, prints a message that cached version is being
## retrieved and returns it; if not, computes inverse and sets cached inverse so it is available if
## needed again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mtrx <- x$getinv_mtrx()
        if(!is.null(inv_mtrx)) {
                message("getting cached data")
                return(inv_mtrx)
        }
        data <- x$get()
        inv_mtrx <- solve(data, ...)
        x$setinv_mtrx(inv_mtrx)
        inv_mtrx
}
}

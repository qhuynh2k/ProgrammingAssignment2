# return list of functions 
# set matrix
# get matrix
# set inverse
# get inverse

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



# checking cached inverse if it is not empty then returns 
# results from cached; otherwise calculate inverse and return result.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

	  # calculate inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

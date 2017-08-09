## makeCacheMatrix is a funtion which creates a list object that has some 'methods' to set and get a matrix, 
## and to set and get its inverse matrix. The 'method' to set the inverse matrix doesn't actually do any validation nor computation:
## it simply caches as inverse matric whatever matrix is passed.
## 
## cacheSolve is a function which computes the inverse of a matrix.
## this funtion requires as input a list object as the one returned by the above makeCachematrix function, checks 
## if an inverse matrix is already present in the cache and if not it computes the inverse, returns it and caches it.
## Note: this funtion can at the moment work only on bidimensional square invertible matrices.

## makeCacheMatrix:  This function creates a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinv <- function(inverse_matrix) inv_x <<- inverse_matrix
        getinv <- function() inv_x
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
        ## check if the matrix is a square bidimensional matrix
        dimensions <- dim(x$get())
        if((length(dimensions)!=2)||(dimensions[1]!=dimensions[2])) {
                message("cannot invert this matrix: matrix has to be a bidimensional square matrix")
                return(NULL)
        }
        ## Return a matrix that is the inverse of 'x'.
        ## check if the inverse is already chached. if so, no computationt needed
        inv_data <- x$getinv()
        if(!is.null(inv_data)) {
                message("getting cached data")
                return(inv_data)
        }
        ## if inverse matrix is not already cached then the inverse is computed, cached, and then returned
        data <- x$get()
        inv_data <- solve(data)
        x$setinv(inv_data)
        inv_data
}

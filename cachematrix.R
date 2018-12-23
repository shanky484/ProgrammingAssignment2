## function intakes a matrix and caches the invesese of the matrix.
## The cached matrix can be used again thereby avoiding repeated calculation.


## makeCacheMatrix inputs a matrix and saves the inverse to cache 

makeCacheMatrix <- function(x = matrix()) {
        invertedmatrix <- NULL
                set <- function(y) {
                        x<<- y
                        invertedmatrix<<- NULL
                        
                }
                 get<- function ()x
                 setInverse <- function(inverse) invertedmatrix <<- inverse
                 getInverse <- function () invertedmatrix
                 list(set = set,
                      get= get, setInverse = setInverse, getInverse=getInverse)
                        

}


# consumes the matrix from makeCacheMatrix and calculated the inverse. If the inverse is already calculated it returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invertedmatrix <- x$getInverse()
        # check if cahce exsist
        if (!is.null(invertedmatrix)) {
                
                message("Since the matix is calculated before getting the data from Cache")
                return(invertedmatrix)
        }
        mat <- x$get()
        invertedmatrix <- solve(mat, ...)
        x$setInverse(invertedmatrix)
        invertedmatrix
        }

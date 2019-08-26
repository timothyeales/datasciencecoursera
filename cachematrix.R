##
## The function makeCacheMatrix takes a matrix <M> as an input and returns a list of functions
## to set/get the input matrix and its cached inverse. e.g. A <- makeCacheMatrix(M).
##
## The associated function cacheSolve takes as an input the list returned by makeCacheMatrix
## and returns the inverse. The inverse is retrived from the cache if avalible or is calculated
## if not. e.g. B <- cacheSolve(A), where A is the return value from makeCacheMatrix.


## This function creates a special "matrix" object that can cache its inverse.
##
## input <x>: Special matrix returned by makeCacheMatrix
## output: list of four component functions which set/get the matrix and the cached inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv_m <- NULL #initalise inverted matrix, inv_m, to NULL
        
        #sets matrix and clears inverse matrix cache
        set <- function(y) {
                x <<- y # x defined with new matrix y
                inv_m <<- NULL # since new matrix, the cache is nulled
        }
        
        #returns the matrix
        get <- function() x
        
        #takes inverted matrix, inv, as input and sets variable inv_m = inv
        set_inv <- function(inv) inv_m <<- inv
        
        # returns the cached value of the inverse matrix
        get_inv <- function() inv_m
        
        #returns list of the four component functions
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.
##
## input <x>: Special matrix returned by makeCacheMatrix
## output <inv_m>: Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        
        inv_m <- x$get_inv() # get stored value of inv_m
        
        # if inv_m not NULL return cached value
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }
        
        # if inv_m NULL calculate inverse and return
        data <- x$get()        # get matrix
        inv_m <- solve(data)   #calculate inverse
        x$set_inv(inv_m)       #set cache value
        inv_m                  #return inverse matrix
}

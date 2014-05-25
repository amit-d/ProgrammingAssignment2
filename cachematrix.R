## makeCacheMatrix and cacheSolve functions in conjuction help to avoid
## repeating computation of inverse of a matrix since such as computations
## can take long time, especially for large matrices.

## Comments adopted from Good explanation by James A. Stephenson (Community TA)
##at link-
## https://class.coursera.org/rprog-003/forum/thread?thread_id=664

## Test cases provided by Gregory D. Horne (Community TA) used from link-
## https://class.coursera.org/rprog-003/forum/thread?thread_id=650



## makeCacheMatrix function-
## Creates a list of subfunctions to manage cache for computation 
## of inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y){
                x <<- y
                I <<- NULL
        }
        get <- function() x
        savecache <- function(solve) I <<- solve
        getcache <- function() I
        list(set=set, get=get, savecache=savecache, getcache=getcache)
}



## cacheSolve function- 
## computes inverse of matrix only if cache does not already contain 
##computation from previous function call for the same matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getcache()
        if(!is.null(I)){
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$savecache(I)
        I
}

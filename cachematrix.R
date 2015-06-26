## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
    ##a object to keep inverted matrix
    xInv <- NULL 
    ##auxillary function returning a main matrix
    get  <- function() x
    ## help function changing a main matrix
    set <- function(y){
        xInv <<- NULL ## new main imply new inv
        x <<- y
    }
    ## return inverted matrix
    getinv <- function() xInv
    ## set new inv matrix
    setinv <- function(y){
        if(is.null(y)) xInv <<-y
        else if(all.equal(dim(y),dim(x))) 
            xInv<<- y
        
    }
    list( get = get, set = set, getinv = getinv,setinv = setinv)
}


## Write a short comment describing this function

cacheSolve<- function(x,...){
    ##check - is inv matrix already in cache
    xInv <- x$getinv()
    if(!is.null(xInv)){
        message("getting cache inv")
        return(xInv)
    }
    ## if not solve new invert matrix
    xInv <- solve(x$get())
    ## change xInv in cache
    x$setinv(xInv) 
    xInv 
}
        ## Return a matrix that is the inverse of 'x'

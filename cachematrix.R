## Create a special matrix object that can cache its inverse
## Set the value of m to NULL - provides a default if cacheSolve is not used
## set : Set the value of the matrix
## get : Get the value of the matrix
## setinverse : Set the value of the matrix inverse
## getinverse : Get the matrix inverse
makeCacheMatrix <- function(x = matrix())  {
    m <<- NULL
    set <- function(y) {
        x<<-y        ## caches the inputted matrix to CacheSolve and check if it has changed
        m<<-NULL     ## sets the value of m (the matrix inverse of cacheSolve) to NULL
    }
    get <- function()x
    setinverse <- function(inverse)
    m <<- inverse         ## cache the matrix inverse
    getinverse <- function()m
    list(set=set, get=get,
    setinverse = setinverse,
    getinverse = getinverse)
}



## Return a matrix that is the inverse of 'x'
## Check if inverse has been calculated
## If inverse is cached return the inverse from cache
## If inverse is not cached get matrix
## Calculate matrix inverse
## Set matrix inverse
cacheSolve <- function(x, ...)  {
    m <- x$getinverse()
    if(!is.null(m))  {
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data, ...)
    x$setinverse(m)
    m
}

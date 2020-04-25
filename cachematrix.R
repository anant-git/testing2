## matrix inverse calculations and storing in cache if already computed before
## helps in elimination of recalculation of results

## to create a special matrix that cache its inverse

makeCacheMatrix <- function(m=matrix()){
    x <- NULL
    set <- function(matrix){
        m <<- matrix
        x <<- NULL
    }
    get <- function() m
    setInverse <- function(inverse) x <<- inverse
    getInverse <- function() x
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## compute the inverse of matrix if not already computed

cacheSolve <- function(y, ...){
    m <- y$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- y$get()
    m <- solve(data)%*%data
    y$setInverse(m)
    m
}

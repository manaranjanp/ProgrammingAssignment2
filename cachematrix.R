## This script contains two functions which enables to calculate inverse of a
## matrix once and cache it. Inverse of the matrix is never calculated it the 
## matrix is not changed.

## makeCacheMattrix() - create a matrix, which also caches its inverse. 
## It takes a matrix as an arguement and caches it.
## Usage:  cm <- makeCacheMatrix( matrix( 1:16, 4, 4) )
## The matrix can be retrieved using cm$get() 
## and the matrix inverse can be retrived using cm$getinverse()
makeCacheMatrix <- function(x = matrix()) {
        
        minv <- NULL
        
        ## When matrix content is set or altered, reset the inverse
        ## of the matrix
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inv) minv <<- inv
        getinverse <- function() minv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## cacheSolve() - calculates the inverse of the matrix passed to it and caches it
## in the matrix crated by makeCacheMatrix(). If the inverse is already available
## in cache or it calculates and stores it in cache and then return it. 
## Assumption: The function assumes that the matrix is invertible.
## Usage:  cm <- makeCacheMatrix( matrix( 1:16, 4, 4) )
##         cmi <-  cacheSolve( cm )
## The matrix inverse can be retrived using cm$getinverse()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinverse()
		
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        
		minv <- solve( x$get() )
        x$setinverse(minv)
        minv
}


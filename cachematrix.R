##
## Title:   R Programming Assignment 2
## Author:  Mike Sadowski
## Created: 20150424
##
## Purpose: Show how Lexical Scoping in R works by writing code which
##          solves and caches the inverse of a matrix so that
##          future fetches use the cached result improving performance
##


## Create four functions bound to the input matrix and returns them in a list
##    set:        initialze given a matrix
##    get:        fetch the matrix itself
##    setinverse: initialze the matrix inverse
##    getinverse: fetch the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
   ## initialize inverse for the matrix to NULL
   im <- NULL

   ## function to set the matrix and initialize the inverse to NULL
   set <- function(matrix) {
      x <<- matrix
      im <<- NULL
   }

   ## function to return the matrix itself
   get <- function() x

   ## function to set the inverse of the matrix
   setinverse <- function(inverse) im <<- inverse

   ## function to return the inverse of the matrix
   getinverse <- function() im

   ## return list of defined functions
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Solves for the inverse of a matrix and caches the result.  If already cached
## returns the cached result
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'.  If calculated earlier
   ## then return the cached version otherwise calculate

   ## Attempt to get the inverse from the cache
   im <- x$getinverse()
   if(is.null(im)) {
      ## Is NULL so no cached inverse; get the matrix and solve
      matrix <- x$get()
      im <- solve(matrix)
      x$setinverse(im)
   }
   ## Return either the calculated or cached inverse of the matrix
   return(im)
}

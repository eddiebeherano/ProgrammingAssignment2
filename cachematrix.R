
##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##cacheinverse : set the inverse of the matrix into cache m variable.
##getcacheinverse: retrieves the value of the matrix inverse from cache. 

makeCacheMatrix <- function( x=matrix() ) {

m<- NULL

set <- function (y) {
  x<<- y
  m<<- NULL
 }

get <- function() x

getcacheinverse <- function () m

cacheinverse <- function (inverse) m <<- solve(x)

list(set = set, get = get,
 	getcacheinverse = getcacheinverse,
	 cacheinverse = cacheinverse)

}


##The second function, cacheSolve reads our special "matrix",
## it reads in the value of the inverse matrix cached to the variable m,
## it then checks whether there is a cached value inside the "m" variable, if the inverse
## of the matrix is cached it returns the value of m, if it isn't it solves for the matrix inverse,
## caches the inverse and it returns the inverse of the matrix stored in "m".



cacheSolve <-function(x, ...) {

m<- x$getcacheinverse()

if(!is.null(m)) {
 message ("getting cached data")
 return(m)
 }
 data <- x$get()
 m <- solve(data,...)
 x$cacheinverse(m)
 m

}

# test cases:

amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()         # Returns original matrix
cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
amatrix$getinverse()  # Returns matrix inverse
cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
amatrix$get()         # Returns matrix
amatrix$getinverse()  # Returns matrix inverse

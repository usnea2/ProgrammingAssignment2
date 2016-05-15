## These functions create a matrix that can cache its inverse, then
##returns that inverse if already cached. If not already cached, it
##computes the inverse and caches it to be used later. 

## This function creates a matrix
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m<- NULL
	set<- function(y){
		x<<-y
		m<<-NULL
	}
	get<- function() x
	set_inverse<- function(solve) m<<- solve
	get_inverse<- function() m
	list(set=set, get=get, set_inverse= set_inverse, get_inverse= get_inverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated, then
## `cacheSolve` retrieves the inverse from the cache. If not, it 
## calculates the inverse of the matrix and stores it in the cache
## above.

cacheSolve <- function(x, ...) {
       m<- x$get_inverse()
       if(!is.null(m)){
       		message("getting cached matrix")
       		return(m)
       }
       data<-x$get()
       m<- solve(data, ...)
       x$set_inverse(m)
}

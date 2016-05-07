## The two functions here are used to create a special matrix that stores a matrix
## and caches its inverse. 

## The function makeCacheMatrix creates a special "matrix" which is in reality
## simply a list holding a function that:
## 1. Sets the value of the matrix
## 2. Gets the value of the matrix
## 3. Sets the value of the inverse
## 4. Gets the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
	s<-NULL
	set<-function(y){
		x<<-y
		s<<-NULL
	}
	get<-function() x
	setMatrix<- function(solve) s<<-solve
	getMatrix<- function() s
	list(set=set, get=get,
		setMatrix=setMatrix,
		getMatrix=getMatrix)
}


## This function solves for the inverse matrix of the special "matrix" created with
## makeCacheMatrix. However, it first determines whether or not the mean has already
## been computed or not. If so, it simply gets the inverse of the matrix from the cache 
## and skips the computation, making things faster. It will return a message stating
## "getting cached data" with the inverse matrix if this is the case. If it must still be computed,
## then it calculates the inverse of the matrix and records this inverse in the cache using the 
## setMatrix function. 

cacheSolve <- function(x, ...) {
        s<-x$getMatrix()
        if (!is.null(s)){
        	message("getting cached data")
        	return(s)
        }
        data<-x$get()
        s<-solve(data, ...)
        x$setMatrix(s)
        s

}
## This function returns a matrix that is the inverse of "x", the original matrix we
## wanted the inverse of. 
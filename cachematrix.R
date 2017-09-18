## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## This function create a special vector which contain a list of a function to
## 1.set data of the matrix
## 2.get data of the matrix
## 3.set inverse data of the matrix
## 4.get inverse data of the matrix(which store in a different environment) 

makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
		set <- function(y){
			x <<- y
			inverse <<- NULL		
		}
		get <- function() x ##Return data of the matrix
		setinverse <- function(inv) inverse <<- inv ##Store inverse data of the matrix x (in a different environment)
		getinverse <- function() inverse ##Return data of the matrix
		##Create a list
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function return an inverse data of the matrix 
## By checking if there is a cached data or not
## If there is a data which is calculated before, return cached data
## If not, calcuate an inverse matrix

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) { ##If this matrix has been calculated, looking for a cached data
                message("getting cached data")
                return(inverse) ##return cached data without new calculation
        }
		##If not(no cached data), calculate an inverse matrix
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}

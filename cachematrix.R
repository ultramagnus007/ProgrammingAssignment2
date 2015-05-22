## Return a list contaning functions for get and set the matrix and its inverse
## it takes matrix x as its argument
## matrix and it inverse are stored in 'x' and 'inv' variable repectively
makeCacheMatrix <- function(x = matrix())
{
	inv <- NULL
	set <- function(y)
	{
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return a matrix that is the inverse of matrix.
## it takes an Object created by makeCacheMatrix() as its parameter
## if matrix inverse is already calculated, then matrix inverse is not computed again and previously calculated inverse is returned
## otherwise matrix inverse is calculated and returned
cacheSolve <- function(x, ...)
{
	inv <- x$getinverse()
	if(!is.null(inv))
	{
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}

#	above functionality can be tested as
#	M <- matrix(c(75, 92, 72, 94) , 2, 2)
#	a<-makeCacheMatrix(M)
#	a$getinverse()
#	cacheSolve(a)
#	cacheSolve(a)
#	a$getinverse()



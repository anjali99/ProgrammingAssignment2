## The purpose of this R software is to provide an efficient way to cache a matrix and its inverse
##Two functions:
##1)makeCacheMatrix 
##2)cacheSolve

## The first function, makeCacheMatrix creates a special "matrix", which contains a function to
## set the value of the matrix,get the value of the matrix,set the value of the inverse matrix,get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
			m <- NULL
			set <- function(y)
			{
				x <<- y
				m <<- NULL
			}
				get <- function() x
				setinverse <- function(inverse) m <<- inverse
		 		getinverse <- function() m
				list(set = set, get = get, setinverse = setinverse, getinverse = getinverse

			}

## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
		if(!is.null(m)) {
					message("getting cached data")
					return(m)
				    }
					data <- x$get()    # get the existing matrix in the cache
					m <- solve(data, ...)  # calculate the inverse
					x$setinverse(m) #store the new inverse in the cache
					m   #return the new inverse
				    }





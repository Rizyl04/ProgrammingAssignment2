##There are two function that applies the following cache regarding the inverse of matrix
##makeCacheMatrix containing the set,get,setinverse,getinverse
makeCacheMatrix <- function (x = matrix()) {
	A <- NULL
	set <-function(y) {
		x <<- y
		A <<- NULL
	}
	get <- function() x	#function to get matrix
	setmean <- function(mean) A <<- mean
	getmean <- function() A
		inverse<-ginv(A)
	inver%*%A
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
	}

##The inverse to matrix function are return. If
##The inverse funtion is calculated. If so, gets the outcome and skips
##calculation. If not, it will calculate the inverse, the calue set will be in cache via
##setinverse function

cacheSolve <- function(x,...){
	A <- x$getinverse()
	if(!is.null(A)){
	message("Cached data")
	return(A)
	}
	matrix<- x$get()
	A <- solve(matrix,...)
	x$setA(A)
	A
	}
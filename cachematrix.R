makeCacheMatrix <- function(x = matrix()) 
{
	invMA 	<- NULL 													#def. inverted matrix NULL
	setMA 	<- function(y) 												#set matrix
		{											
            x <<- y
            invMA <<- NULL
		}
	getMA 	<- function() x												#get matrix

	setinv 	<- function(inverse) invMA <<- inverse 						#set inv matrix
    getinv 	<- function() invMA 										#get inv matrix
	list(set=setMA, get=getMA,CAsetinv=setinv,CAgetinv=getinv)  		#for later access
}

cacheSolve 	<- function(x, ...) 
{
    invMA	<-x$CAgetinv()												#try loading inv matrix from cache
	if(!is.null(invMA))													#if cache is there return it and leave function
		{
            message("get inv matrix from cache")
            return(invMA)
		}
			message("calculation required")
	newMA 	<-x$get()													#calc inv matrix
	invMA	<-solve(newMA,...)
	x$CAsetinv(invMA)													#cache inv matrix
	invMA
}

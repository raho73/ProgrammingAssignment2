##### makeCacheMatrix function ###############################
makeCacheMatrix <- function(x = matrix()) 
{
  #def. inverted matrix NULL
  invMA <- NULL
  
  #set matrix
	setMA<- function(y) 											
		{											
      x<<- y
      invMA <<- NULL
		}
	
	#get matrix
	getMA<- function() x												
	
	#set inv matrix
	setinv<- function(inverse) invMA <<- inverse
	
	#get inv matrix
  getinv<- function() invMA
  
  #for later access
	list(set=setMA, 
	     get=getMA,
	     CAsetinv=setinv,
	     CAgetinv=getinv)  		
}
##### end makeCacheMatrix ####################################

##### cacheSolve function ####################################
cacheSolve 	<- function(x, ...) 
{
  #try loading inv matrix from cache 
  invMA<-x$CAgetinv()
  
  #if cache is there return it and leave function
	if(!is.null(invMA))													
    {
      message("get inv matrix from cache")
      return(invMA)
    }
  message("calculation required")
	 
	#calc inv matrix
	newMA<-x$get()													
	invMA<-solve(newMA,...)
	
	#cache inv matrix
	x$CAsetinv(invMA)
	
	#return inv matrix
	invMA
}
##### end cacheSolve #########################################
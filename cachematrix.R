## these two functions work together to cach the inverse of the matrix for later retrieval.  I'm a little 
##unsure as to wheter i also should should compare the cached version of the original matrix to the current state of that matrix

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) m<<- solve
        getinverse<-function() m
        list(set=set, get=get,name=x,
             setinverse=setinverse,
             getinverse=getinverse)
}



## This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get() 
        m<-solve(matrix, ...)
        x$setinverse(m)
        m
}

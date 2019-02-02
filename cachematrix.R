##Coursera R course week3 Assignment

## makeCacheMatrix():creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y  ##assign a value to an object called X in an environment different from the current environment
                inv<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) inv<<-inverse
        getinverse<-function()inv
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
        
}




##cacheSolve(): computes the inverse of the "matrix" returned by makeCacheMatrix(). 
##If the inverse has already been calculated and the matrix has not changed, it'll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){
                ## skip computation if the inverse has been calculated
                message("getting cached data")
                return(inv)
        }
        
        #Or, continue the calculation
        data<-x$get()
        inv<-solve(data,...)
        x$setinverse(inv)
        
        return(inv)
}


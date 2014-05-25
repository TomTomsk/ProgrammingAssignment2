## These functions will cache the inverse of a matrix. First a special matrix object is created that can cache the inverse, 
## after which the inverse of the special matrix object is calculated. 
## Note that they work similar to the example in the assignment, where cached the mean of a vector. The same approach
## is used, but in this case the inverse of the matrix can be cached.

## The makeCacheMatrix creates a special matrix object that can cache its inverse. 
## First the value of the matrix is set, then I get the value of the matrix. 
## Then I set the value of the special matrix and actually get that special matrix.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## This function calculates the inverse of the matrix, even though it first checks to see if that has already been calculated. 
## If it has not, it sets the value of the inverse matrix by the setmatrix command.

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}

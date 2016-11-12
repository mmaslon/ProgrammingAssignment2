## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## I create list of fucntion to be used later by cache solve
makeCacheMatrix <- function(x = matrix()) {
    #i create a variable that will store inverted matrix, starts as null
   matrix_inverse<-NULL
   #create matrix in the parentg environment
    set<-function(y){
        x<<-y
        matrix_inverse<<-NULL
    }
    #get value of matrix
    get<-function() x
    #invert matrix and store the result
    setinversion<-function(inversion) matrix_inverse<<-inversion
    ##get the inverted matrix from cache
    getinversion<-function() matrix_inverse
    #returns  an object for use in cacheSolve
    list(set=set, get=get, setinversion=setinversion, getinversion=getinversion)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inverse<-x$getinversion()
        ##return inverted matrix if exist
        if(!is.null(matrix_inverse)) {
            message("getting cached data")
            return(matrix_inverse)
        }
        ##if not create matrix
        data<-x$get()
        ##creare inversion, will only work for square matrces
        matrix_inverse<-solve(data, ...)
        x$setinversion(matrix_inverse)
        matrix_inverse
}

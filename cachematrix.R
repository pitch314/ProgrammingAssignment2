#### R programming, Programming Assignment 2: Lexical scoping
##Caching the inverse of a matrix
# ------------------------------------------------------------------------------#
# To write an R function is able to cache potentially time-consuming computations.
# Matrix inversion is usually a costly computation and their may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly.
# Your assignment is to write a pair of functions that cache the inverse of 
# a matrix:
#   makeCacheMatrix: This function creates a special "matrix" object that can
#                     cache its inverse.
#   cacheSolve:      This function computes the inverse of the special "matrix"
#                     returned by makeCacheMatrix above. If the inverse has 
#                     already been calculated (and the matrix has not changed),
#                     then the cachesolve should retrieve the inverse from the cache.
# ------------------------------------------------------------------------------#

###
## Function giving an object with remain variable that can perform result cached
##  operation like matrix inversion.
##
## @param x     (Should be a matrix or coercive matrix object) Matrix to convert
##
## @result      an list of function manipulating the 'x' matrix given and its
##               cached element
###
makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL;  #remain variable. Will be use to store matrix inverse of 'x'
    
    #Function to replace current matrix by a new given matrix 'y'
    # (and reset the remain/cached variable)
    set_function <- function(y) { x <<- y; inv_m <<- NULL; };
    
    #Function giving current matrix manipulated for cached operation
    get_function <- function() { x; };
    
    #Function to store cached operation result and the remain variable
    setcachedvariable <- function(result) { inv_m <<- result; };
    
    #Function giving the remain variable
    getcachedvariable <- function() { inv_m; };
    
    list(set = set_function, get = get_function,
         setmean = setcachedvariable, getmean = getcachedvariable) #return of makeCacheMatrix
}

###
## Function performing matrix inversion operation of object created by function 
##  makeCacheMatrix and cache result in the object.
##
## @param x     special matrix (object created by makeCacheMatrix() )
## @param ...   additional argument for solve function
##
## @result      a matrix that is the inverse of 'x'
###
cacheSolve <- function(x, ...) {
    m <- x$getmean(); #Get cached inverse of 'x' if exist
    
    if(!is.null(m)) { #Do we calculate matrix inverse ?
        message("getting cached data"); #no need
    } else {
        m <- solve(x$get(), ...);
        x$setmean(m); #cache the result in the remain variable
    }
    
    return(m);
}


#################################################################################
# Give a invertible matrix of 'Nd' dimension
inversibleMatrix <- function(Nd=2000) {
    set.seed(6463);
    U  <- matrix(rnorm(Nd^2, mean=1, sd=2), nrow=Nd);
    V  <- matrix(rnorm(Nd^2, mean=3, sd=1), nrow=Nd);
    Di <- runif(Nd, min=.2, max=6.3);
    
    X <- U %*% diag(sort(Di)) %*% t(V);
    return(X);
}

testMatrix <- function(X, n=2) {
    b<-data.frame(solve=0, cacheSolve=0);

    Y<-makeCacheMatrix(X);

    for(i in 1:n){
        a<-Sys.time();
        solve(X);
        b[i,1]<-difftime(Sys.time(), a, units="secs");
        
        a<-Sys.time();
        cacheSolve(Y);
        b[i,2]<-difftime(Sys.time(), a, units="secs");
    }
    
    return(b);
}
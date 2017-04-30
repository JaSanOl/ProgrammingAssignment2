## Put comments here that give an overall description of what your
## functions do
##I have 2 functions, the 1st function, "makeCacheMatrix" initializes the value of x and i, that
##is, of the matrix and the inverse, then it stores the values of x and of i. the 2nd function
##"cacheSolve" checks whether there already has been calculated an inverse and retrieves the
##inverse from the cache if this is so. Otherwise it calculates the inverse and assigns it to
## the variable "i" which is returned by the function.

## This function, first initializes i to be NULL and x is the matrix that will be introduced by
##the user. Then it sets the value of i and x, so that in case you had other values stored in
##memory these values will be replaced. It gets the value of x. Finally, the function sets the
##inverse as the variable "i" and returns "i" also with the "getter-function". The function
##returns a list that gives the values of the "set", "get","getinverse" and "setinverse" function.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i<<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


##This function gets "i" from the "getinverse" function of the makeCacheMatrix-function.
##Afterwards, it checks if it is NULL. If it isn't, this means the inverse has been calculated
## and it will return "i". If the inverse does not exist yet, it gets the data through the "get"
##function, and it calculates the inverse of the matrix which is stored in "data" through the
## "solve" command. Then it assigns the inverse to the variable "i"  through "setinverse", so
## that at this point the function stores the inverse in the cache memory and then it returns "i".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        if(!is.null(i)){
                message("getting cached data")
                return (i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Here I tried out an example
matriz<- matrix(c(1, 3.4, 2.1, 8), 2,2)
inversa_matrix <- solve(matriz)


matriz_cache <- makeCacheMatrix(matriz)
cacheSolve(matriz_cache)






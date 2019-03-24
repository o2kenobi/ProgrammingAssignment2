#read:
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.mdsource("FuncHW.R")
# Yuling Wu (o2kenobi), March 24,2019 (Modified from Assgnm Ex)
#
#=======================================================
#=The funcion defines 4 functions for setting/getting object x (matrix) and its inverse (mi)
#= and sets initial mi to NULL and x from input arg
#=======================================================
makeCacheMatrix <- function(x = numeric()) {
   mi <- NULL
   set <- function(y) {
      x  <<- y                             #<-- assign x  in parent env
      mi <<- NULL                          #<-- assign mi in parent env (as x is reset, so is mi)
   }
   get <- function() x                     #<-- get x from parent env
   setMinv <- function(Minv) mi <<- Minv   #<-- put input arg Minv to mi (in parent env)
   getMinv <- function() mi                #<-- get mi from parent env
   list(set = set, get = get,              #<-- make a list of the 4 functions and return them
        setMinv = setMinv,
        getMinv = getMinv)
}
#=======================================================
#=The funcion calcs the inverse of a matrix when it first is called 
#=  upon (mi hasn't been calculated, =NULL) to get the inverse, and 
#=  puts the calculated inverse in cache; after that(mi is no longer
#=  NULL), when it'a called, it gets the inverse from cache
#=======================================================
cacheSolve <- function(x, ...) {
   mi <- x$getMinv()                 #<-- call getMinv to get mi (initially is null)
   if(!is.null(mi)) {                #<-- if mi not null (already has value)
      message("getting cached data")
      return(mi)
   }
   data <- x$get()                   #<-- if mi is null, get matrix data (x)
   mi <- solve(data,...)             #     calculate the inverse
   x$setMinv(mi)                     #<-- then set it on cache
   mi                                # and return this calculated inverse
}


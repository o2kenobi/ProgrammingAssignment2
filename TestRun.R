#---Test run the functions...
source("cachematrix.R")
mx <- makeCacheMatrix(matrix(rnorm(9),3,3))
cacheSolve(mx)
cacheSolve(mx)  # the inverse is obtained from cache this time.

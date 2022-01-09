## 
## Function makecachematrix
## Creates a cache of the solved matrix inverse
## and leaves the computed values in a vector 
## 
## It makes use of a list containing functions to
## 1. set the value of a matrix, 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## using the solve() function
## and finally, 4. get the value of the matrix inverse


makecachematrix <- function(x = matrix())
{
  
  inverse_m <- NULL
  
  set <- function(y) {
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inverse_m <<- inverse
  get_inverse <- function() inverse_m
  list(set = set, 
       get = get,
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
}


## Function cachesolve

## First checks if the inverse of the matrix
## already exists in the cache 
## If not, then calculate the inverse of a matrix
## and set the value of the inverse in the cache
## for future use

cachesolve <- function(x, ...)
{
  inv <- x$get_inverse()
  
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$set_inverse(inv)
  inv
  
}

#
# test matrices for verification

#b = matrix(c(-1,1,3/2,-1),2,2)

#b=matrix(c(1,2,3,4),2,2)
#b
#b1 <- makecachematrix(b)
#i<-cachesolve(b1)
#i

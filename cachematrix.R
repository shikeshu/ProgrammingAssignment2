makeCacheMatrix <-function(x=matrix(data, nrow, ncol, byrow), Inv=NULL)
{
## first condition checks if the matrix is square by checking if it has equal number of column and row.  
##  It also checks if matrix is not singular
 if (nrow(x)==ncol(x) && det(x)!=0) {
matequal <- function(x, y) 
        { all(x == y) }
 matInv <-Inv
  ## here it checks the inverse is provided and if it is a correct inverse of the matrix if yes it stores its value
 if (!is.null(matInv)) {matInv <- matInv
 if (matequal(zapsmall(x %*% matInv), diag(nrow(x)))) {}
else {matInv <- NULL}
}

## here x and y are objects sets the value of x  matrix and its inverse
set<-function(y) {
 x<<-y
matInv<<-NULL
}

## get function calls the value of x matrix
get<-function() {x}
##setInv creates and object to take the value of matrix inverse
setInv <-function(Inv) {matInv <<- Inv}
 ## getInv function returns the value of the matrix inverse
 getInv <-function() {matInv}
 #list sets the value for x matrix , gets the value of x matrix, sets the value of x matrix inverse and gets the value of Matrix Inverse
 list(set=set,get=get,setInv=setInv, getInv=getInv)
}
## if matrix was not square or invertible following message appears
else {message(" either Not a square Matrix or Not invertible ")}
}

cacheSolve <- function(x, ...) {
 ## MatInv first retrieves the value from the Cache
        matInv <- x$getInv()
  ## checks if the MatInv is not null and if not null retrieve the value from Cache data 
         if(!is.null(matInv)) {
                message("getting cached data")
                 return(matInv)
        }
 ## if MatInv is Null data calls the x Matrix by calling get function 
        data <- x$get()
  ## calculates the matrix inverse using solve function and caches in MatInv variable
  matInv<-solve(data)
 ## x$setInv  set the value of matrix invese in the object
        x$setInv(matInv)
## zapsmall rounds the value of the matrix and return the inverse 
      zapsmall(matInv)
  
}

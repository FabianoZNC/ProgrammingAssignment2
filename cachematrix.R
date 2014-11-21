## > matrix
##      [,1] [,2]
## [1,]    2    3
## [2,]    2    5

##> det(matrix)
## 4

##> solve(matrix) %*%  matrix
##       [,1] [,2]
##  [1,]   1    0
##  [2,]   0    1


## This function creates a special
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            m<-NULL
            set<-function(y){
                x<<-y
                m<<-NULL
            }
            get<-function() x

            set_matrix<-function(solve) m<<- solve
            get_matrix<-function() m
            list(
                set=set,
                get=get,
                set_matrix=set_matrix,
                get_matrix=get_matrix
                )
}


## This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x=matrix(), ...) {
    m<-x$get_matrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$set_matrix(m)

    m
}


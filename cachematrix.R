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
### to test uncomment the next line, and run cachematrix.R
##cacheSolve(makeCacheMatrix(matrix(c(2 , 2, 3, 5), 2, 2)))

## > m <- matrix(c(2 , 2, 3, 5), 2, 2)
## > m
##      [,1] [,2]
## [1,]    2    3
## [2,]    2    5

##> det(m)
## 4
#### Ok, determinat > 0
### if det(m) == 0 , not Ok

##> solve(m) %*%  matrix
##       [,1] [,2]
##  [1,]   1    0
##  [2,]   0    1
### identity

## > m_inverse <- solve(m)
## > m_inverse
##       [,1]  [,2]
## [1,]  1.25 -0.75
## [2,] -0.50  0.50

### To prove
## m%*%m_inverse
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1

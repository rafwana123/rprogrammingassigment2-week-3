makeCacheMatrix <- function(x = matrix(sample(1:100,9), 3,3)) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() ms
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting inversed matrix")
                return(s)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(s)
        s
}
makeCacheMatrix<- function(x=matrix()){
inv<-NULL
set<- function(y){
x<<-y
inv<<- NULL
}
get<- function()x
setinv<-function(inverse)inv<<-inverse
getinv<-function()inv
list(set = set, get = get, setinv = setinv, getinv = getinv)
}
cacheSolve<- function(x,...){
##Return a matrix that is the inverse of 'x'
inv<-x$getinv()
if(!is.null(inv)) {
message("getting cached result")
                return(inv)
}
data<- x$get()
inv<- solve(data,...)
x$setinv(inv)
inv
}
 
cacheSolve <- function(x, ...) {
s <- x$getsolve()
if(!is.null(s)) {
 message("getting inversed matrix")
return(s)}
 data <- x$get()
m <- solve(data, ...)
x$setsolve(s)
s}

makeCacheMatrix<- function(x=matrix()){
inv<-NULL
set<- function(y){
x<<-y
inv<<- NULL}
get<- function()x
setinv<-function(inverse)inv<<-inverse
getinv<-function()inv
list(set = set, get = get, setinv = setinv, getinv = getinv)
}

data<- x$get()
inv<- solve(data,...)
x$setinv(inv)
inv }
my_matrix<- makeCacheMatrix(matrix(1:4,2,2))
my_matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
my_matrix$getinv()
NULL
cacheSolve(my_matrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
cacheSolve(my_matrix)
getting cached result
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
my_matrix$getinv()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
my_matrix$set(matrix(c(1:8,2,4))
+ )
my_matrix$get()
      [,1]
 [1,]    1
 [2,]    2
 [3,]    3
 [4,]    4
 [5,]    5
 [6,]    6
 [7,]    7
 [8,]    8
 [9,]    2
[10,]    4
> my_matrix$getinv()
NULL
> cacheSolve(my_matrix)
Error in solve.default(data, ...) : 'a' (10 x 1) must be square
> my_matrix$set(matrix(c(2,2,1,4),2,2))
> my_matrix$get()
     [,1] [,2]
[1,]    2    1
[2,]    2    4
> my_matrix$getinv()
NULL
> cacheSolve(my_matrix)
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> cacheSolve(my_matrix)
getting cached result
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> my_matrix$getinv()
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
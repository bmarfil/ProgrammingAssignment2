## Put comments here that give an overall description of what your
## functions do

## This function will create and cache the inverse of a matrix object
makeCacheMatrix <- function(x = matrix()) {                                             #The X argument will have to be a matrix object
        s<-NULL                                                                         #An empty object, "s" (for solve) is created to be used later in the function
        set<-function(y=matrix()){                                                       #Set function is created, a matrix default category was set 
                x<<-y                                                                   #Assign the "y" value of the set function to the "x" object in the parent directory (hence the <<- assignment operator)                                                          
                s<<-NULL                                                                #Assign the "NULL" value of the set function to the "s" object in the parent directory (hence the <<- assignment operator) 
        }
        get <- function() x                                                             #Get function is created to get the "x" object, which is a matrix 
        setsolve <- function(solve) s <<- solve                                         #setsolve function is created to set the inverse of a matrix, using the "solve" function. Assign "s" value to parent directory (using <<- assignment operator)
        getsolve <- function() s                                                        #getsolve function is created the get the inverse of the matrix (from the setsolve function) 
        list(set = set, get = get,                                                      #Return a list of the funtions to be able to refer to functions using the "$" operator
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function will calculate the inverse matrix returned by "makeCacheMatrix". If the
# inverse is already calculated and the data didn't change, the "cacheSolve" will retrieve
# the inverse from the cache

cacheSolve <- function(x, ...) {
        s<-x$getsolve()                                                                 #Return a matrix that is the inverse of "x"
        if(!is.null(s)){                                                                #Return the inverse if it is already set (!is.null)
                message("getting cached data")
                return(s)
        }
        data<-x$get()                                                                   #Get the matrix from object (i.e. previously created matrix)
        s<-solve(data, ...)                                                             #Calculate the inverse 
        x$setsolve(s)                                                                   #Set the inverse to the object "s"
        s                                                                               #Return "s" object (in this case, a matrix)
}

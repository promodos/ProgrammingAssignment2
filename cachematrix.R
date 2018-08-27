# Caching the Inverse of a Matrix:
# Matrix inversion beansprucht Rechenleistung und deshalb
# ist es vorteilhaft, eine einmal errechnete Inverse Matrix zu speichern (cache)
# anstatt diese immer aufs Neue zu berechnen..

# Nachfolgend stehen Funktionen um ein spezielles Objekt zu schaffen, das
# eine übergebene Matrix im Cache abspeichert und ihre zugehörige inverse Matrix berechnet, um diese
# ebenfalls im Cache zur Verfügung zu stellen.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
# Diese Funktion berechnet die inverse Matrix der im Cache abgelegten speziellen Matrix, von 
# der Funktion makeCacheMatrix erstellt worden ist. Wenn die inverse Matrix bereits berechnet worden ist und 
# unverändert vorliegt, wird die inverse Matrix aus dem Cache geholt.

cacheSolve <- function(x, ...) {
#        inverse matrix von 'x'  zurückgeben 
        
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

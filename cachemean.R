makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
                setmean = setmean,
                getmean = getmean)
}
'''
The following function calculates the mean of the special "vector"
created with the above function. However, it first checks to see if the
@@ -50,17 +52,19 @@ cache and skips the computation. Otherwise, it calculates the mean of
the data and sets the value of the mean in the cache via the `setmean`
function.
'''
cachemean <- function(x, ...) {
        m <- getmean(x)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

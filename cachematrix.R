#This is a R function which has two variables to store input matrix and inverse of the 
#matrix. This function contains Getter/Setter method for input matrix. Getter/Setter for matrix inverse.
makeCacheMatrix <- function(input = matrix()) {

      matrix_inv <- NULL 
	# Setter function
      set <- function(y) {
	  input <<- y
	  matrix_inv <<- NULL 
      }

	#Getter function. Returns input matrix
      get <- function() {
		input 
	}

	#Matrix Inverse setter. Sets the parent matrix inverse variable matrix_inv
      setInv <- function(inv) {

	 	matrix_inv <<- inv 

	}

	#Get the matrix_inv which already cached
      getInv <- function() {

		matrix_inv
	}
      # Returns all functions defined in this parent fucntion as a list

       list(set = set, get = get, setInv = setInv, getInv = getInv)
  }

#This function uses above function Object which has input matrix. Retrieves the inverse of this input
# matrix if the inverse is NULL then calculates and store its back in that function object for later use
cacheSolve <- function(m, ...) {
	#Get the inversed matrix from object m which uses R object makeCacheMatrix 
      matrix_inv <- m$getInv() 

      # If matrix_inv not NULL then it is from cache or else calculate it and store it back into m object using setInv function
      if(!is.null(matrix_inv)) { 
	  	message("Fetched Matrix Inverse from cached data")
	  	
      } else {
      	input_matrix <- m$get() 
      	calculated_matrix_inverse <- solve(input_matrix) 
	      m$setInv(calculated_matrix_inverse)
		message("Calculated, stored in the cache and fetching it from cache")
      	matrix_inv <- m$getInv()
	
	}
	return(matrix_inv)
  }

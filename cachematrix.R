#
# first function requied to create the list of fuctions 
#
MakeCacheMatrix<- function (x=numeric(),dimension)
{ # the only command that will run when you call this function is the last - list()
  # just to construct a list of switches of the function MakrCacheMatrix()
    
      
        # sub-function to create the matrix in the global space across the board
	  makematrix <- function(y=x, dim2=dimension)
	  {
	      X <<- matrix (y, nrow = dim2,ncol = dim2) 
	      Xinv <<- NULL
	  }

        
	# sub-function to retrieve the matrix from the global variable
          getmatrix <- function() {return(X)}
			  
	
	# sub-function to set the inverse globally across the board
	  setinv <- function(x) {Xinv <<- x}                                                               

	# sub-function to get the inverse from the global variable
	  getinv <- function() {Xinv}

	
	# the only command that will run when you call Makecachematrix() is the next one
	# just to construct a list the different switches of the function

          list( makem = makematrix , getm =getmatrix , getinv =getinv , setinv =setinv)
 }
	
#
# second function required to retrieve the inverse from the global variable Xinv 
# if Xinv is nulled due to change in the matrix
#
CacheSolve <- function(Z)
{
  #find if the inv is computed already
   if (!is.null(Z$getinv()))
      {
              message("Retrieving cached data")
	      return (Xinv)
      }

  #else solve for the inverse - use solve()!!   

      message("Hang on... solving the matrix")
      data1 <- Z$getm()	            
      data1inv <- solve(data1)
      Z$setinv(data1inv)
      # return the newly computed inverse to the screen
      data1inv
						      	
}




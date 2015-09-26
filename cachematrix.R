## The following two functions cache a matrix inversion so it is possible
## to prevent it from being evaluated twice. This saves computing time.
## This is for coursera data science specialization, the course name is
## R programming This is assigment for week 3 in lexical scoping

## The following function stores the data of the matrix and also
## caches the inverse. 

makeCacheMatrix <- function(x = matrix()) 
  {
  #the caching will be done in the global environment
  #use a function to store the bundle of matrix, id and inverse
  
  ## for first time usage, call makeCacheMatrix and assign its output
  ## to an object. The function will also create a list called bundle
  ## the argument is any invertible matrix 
  ## then call cacheSolve on the to compute the inverse
  
  mtrx <- x #change its name.
  sum.mtrx <- sum(mtrx) #use the sum of the matrix elements to distinguish it
  m.inv<-c() # for every new matrix, evaluate this in function cacheSolve
  bundle<<-list(matrix=mtrx,id=sum.mtrx,inv=m.inv)
  #Now this function reads the bundle and returns it. This function needs
  #to read from the same environment that the set function writes to
  read.matrix <- function() #this function needs not a parameter because it does
  {                 #not compute anything
    x #read the matrix form the bundle
  }
  #The following function writes the inverse on the bundle. Since the 
  #inverse is calculated elsewhere, its argument is the inverse
  write.inv<-function(m.inv)
  { #this uses the superassigment so it writes on the global. Otherwise
    #this creates a local variable  
    bundle$inv<<-m.inv #writes on the corresponding list element
  } 
  #the next function reads the value of the inverse from the 
  #bundle. Since it computes nothing, it does not need an argument
  read.inv <- function()
  {
    bundle$inv
  }
  #now the four functions need storing, so a list is created. 
  #calling an element of the list executes the function. This 
  #list does not contain either data, id nor inv
  list(read.matrix=read.matrix,write.inv=write.inv,read.inv=read.inv)
}


## This function computes the inverse of a matrix, and stores the
## result in a matrix bundle, to save computing time. First checks
## if the matrix is changed.
## the argument of this function is the list of functions returned by the
## makeCacheMatrix function

cacheSolve <- function(x, ...) 
{
  #the matrix stored in the bundle can be changed outside the 
  #functions. To decide if it has been changed, the id at the 
  #moment of creation is used.
  
  tmp.id<-bundle$id
  tmp.mtrx<-x$read.matrix()
  nrowcol<-dim(tmp.mtrx) #This is for checking if square
  #now check if the matrix has been changed. Use the sum and the id
  if (tmp.id==sum(tmp.mtrx))
    message('Matrix is OK')
  else
    error('Matrix has been changed')
  #if there a matrix in the bundle, read it
  if (is.null(x$read.inv())) #checks wether empty
  {
    #checks if matrix is rectangular
    if (nrowcol[1]!=nrowcol[2])
      error("matrix is not rectangular")
    # create local variable for the inverse
    m.inv<-solve(tmp.mtrx)
    #then and store it in the bundle
    x$write.inv(m.inv)
    #display the result
    m.inv
  } 
  else #there is something in the bundle, read the cache
    message("reading from cache")
    x$read.inv()
    
}


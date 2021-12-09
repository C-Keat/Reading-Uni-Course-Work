##########################################################
#Question 1
#Defining the Matrix
A<-matrix(c(3,-1,0,-1,0,-1,3,-1,3,-1,0,-1,3,1,0,-1,3,1,5,-1,0,-1,0,-1,1), nrow=5)

#Function that can be passed Matx in order to perform Cholesky Functions
choleskyFunction <- function(Matx){
  #Finds the upper triangular matrix
  L<-chol(Matx)
  #Transposes the matrix to show the lower triangular matrix
  Lt<-t(L)
  #Displays the lower hand matrix
  Lt
}
#Runs the function
choleskyFunction(A)
##########################################################
#Question 2 
#Singular value Decompostion
#the creation of matrix
Q2Matrix <- matrix(c(1,1,1,1,2,0,0,0),nrow = 4)
#Displaying original matrix
Q2Matrix
#using the svd function to show the singular values
sv <-svd(Q2Matrix)
#Displaying Singular values
sv
##########################################################